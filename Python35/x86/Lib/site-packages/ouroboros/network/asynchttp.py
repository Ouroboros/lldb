from ..common import *
from ..otypes import *
import aiohttp
import asyncio
import http.cookies
import urllib

def canonicalHeaderKey(key):
    return '-'.join([s[0].upper() + s[1:].lower() for s in key.split('-')])

class Request(aiohttp.Request):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.headers = _CaseInsensitiveDict()

    def _add_default_headers(self):
        aiohttp.HttpMessage._add_default_headers(self)
        self.headers[canonicalHeaderKey(aiohttp.hdrs.CONNECTION)] = self.headers.pop(aiohttp.hdrs.CONNECTION)

    def add_header(self, name, value):
        """Analyze headers. Calculate content length,
        removes hop headers, etc."""
        assert not self.headers_sent, 'headers have been sent already'
        assert isinstance(name, str), 'Header name should be a string, got {!r}'.format(name)
        assert set(name).issubset(aiohttp.protocol.ASCIISET), 'Header name should contain ASCII chars, got {!r}'.format(name)
        assert isinstance(value, str), 'Header {!r} should have string value, got {!r}'.format(name, value)

        name = canonicalHeaderKey(name.strip())
        value = value.strip()

        name_upper = name.upper()

        if name_upper == 'CONTENT-LENGTH':
            self.length = int(value)

        if name_upper == 'CONNECTION':
            val = value.lower()
            # handle websocket
            if 'upgrade' in val:
                self.upgrade = True
            # connection keep-alive
            elif 'close' in val:
                self.keepalive = False
            elif 'keep-alive' in val and self.version >= aiohttp.protocol.HttpVersion11:
                self.keepalive = True

        elif name_upper == 'UPGRADE':
            if 'websocket' in value.lower():
                self.websocket = True
                self.headers[name] = value

        elif name_upper == 'TRANSFER-ENCODING' and not self.chunked:
            self.chunked = value.lower().strip() == 'chunked'

        elif name_upper not in self.HOP_HEADERS:
            if name_upper == 'USER-AGENT':
                self._has_user_agent = True

            # ignore hop-by-hop headers
            self.headers.add(name, value)

aiohttp.Request = Request

class _CaseInsensitiveDict(CaseInsensitiveDict):
    def add(self, key, value):
        self[key] = value

    # def items(self, **kwargs):
    #     return super().items()

class _ClientRequest(aiohttp.client.ClientRequest):
    def __init__(self, *args, noQuotoPath = False, **kwargs):
        self.noQuotoPath = noQuotoPath
        super().__init__(*args, **kwargs)

    @classmethod
    def factory(cls, **customArgs):
        def wrapper(*args, **kwargs):
            kwargs.update(customArgs)
            return cls(*args, **kwargs)

        return wrapper

    def update_path(self, params):
        """Build path."""
        # extract path
        scheme, netloc, path, query, fragment = urllib.parse.urlsplit(self.url)
        if not path:
            path = '/'

        if isinstance(params, dict):
            params = list(params.items())
        elif isinstance(params, (aiohttp.MultiDictProxy, aiohttp.MultiDict)):
            params = list(params.items())

        if params:
            params = urllib.parse.urlencode(params)
            if query:
                query = '%s&%s' % (query, params)
            else:
                query = params

        if not self.noQuotoPath:
            path = urllib.parse.quote(path, safe='/%:')

        self.path = urllib.parse.urlunsplit(('', '', path, query, fragment))

    def update_cookies(self, cookies):
        """Update request cookies header."""
        if not cookies:
            return

        c = http.cookies.BaseCookie()
        if aiohttp.hdrs.COOKIE in self.headers:
            c.load(self.headers.get(aiohttp.hdrs.COOKIE, ''))
            del self.headers[aiohttp.hdrs.COOKIE]

        if isinstance(cookies, dict):
            cookies = cookies.items()

        for name, value in cookies:
            if isinstance(value, http.cookies.Morsel):
                # use dict method because SimpleCookie class modifies value
                dict.__setitem__(c, name, value)
            else:
                c[name] = value

        self.headers['Cookie'] = c.output(header='', sep=';', attrs = {}).strip()

    def update_headers2(self, headers):
        """Update request headers."""
        self.headers = _CaseInsensitiveDict()

        if headers:
            if isinstance(headers, dict):
                headers = headers.items()
            elif isinstance(headers, aiohttp.MultiDict):
                headers = headers.items()

            for key, value in headers:
                self.headers[key] = value

        # DEFAULT_HEADERS = {
        #     'Accept': '*/*',
        #     'Accept-Encoding': 'gzip, deflate',
        # }

        # for hdr, val in DEFAULT_HEADERS.items():
        #     if hdr not in self.headers:
        #         self.headers[hdr] = val

        # add host
        if 'HOST' not in self.headers:
            self.headers['Host'] = self.netloc

class ProxyConnector(aiohttp.connector.ProxyConnector):
    def setProxy(self, host, port, login = None, password = None, encoding = 'latin1'):
        self._proxy = 'http://%s:%d' % (host, port)
        if login is not None and password is not None:
            self._proxy_auth = aiohttp.helpers.BasicAuth(login, password, encoding)

    async def _create_connection(self, req, **kwargs):
        import aiohttp.hdrs as hdrs

        proxy_req = _ClientRequest.factory()(
                        hdrs.METH_GET,
                        self._proxy,
                        headers = {hdrs.HOST: req.host},
                        auth    = self._proxy_auth,
                        loop    = self._loop
                    )

        try:
            transport, proto = await super()._create_connection(proxy_req)
        except OSError as exc:
            raise aiohttp.ProxyConnectionError(*exc.args) from exc

        if not req.ssl:
            req.path = '{scheme}://{host}{path}'.format(
                            scheme = req.scheme,
                            host   = req.netloc,
                            path   = req.path
                        )

        if hdrs.AUTHORIZATION in proxy_req.headers:
            auth = proxy_req.headers[hdrs.AUTHORIZATION]
            del proxy_req.headers[hdrs.AUTHORIZATION]
            req.headers[hdrs.PROXY_AUTHORIZATION] = auth
            proxy_req.headers[hdrs.PROXY_AUTHORIZATION] = auth

        if req.ssl:
            # For HTTPS requests over HTTP proxy
            # we must notify proxy to tunnel connection
            # so we send CONNECT command:
            #   CONNECT www.python.org:443 HTTP/1.1
            #   Host: www.python.org
            #
            # next we must do TLS handshake and so on
            # to do this we must wrap raw socket into secure one
            # asyncio handles this perfectly
            proxy_req.method = hdrs.METH_CONNECT
            proxy_req.path = '{}:{}'.format(req.host, req.port)
            key = (req.host, req.port, req.ssl)
            conn = aiohttp.connector.Connection(
                        self,
                        key,
                        proxy_req,
                        transport,
                        proto,
                        self._loop
                    )

            self._acquired[key].add(conn._transport)
            proxy_resp = proxy_req.send(conn.writer, conn.reader)
            try:
                resp = await proxy_resp.start(conn, True)
            except:
                proxy_resp.close()
                conn.close()
                raise
            else:
                conn.detach()
                if resp.status != 200:
                    raise aiohttp.HttpProxyError(code=resp.status, message=resp.reason)
                rawsock = transport.get_extra_info('socket', default=None)
                if rawsock is None:
                    raise RuntimeError("Transport does not expose socket instance")

                transport.pause_reading()
                transport, proto = await self._loop.create_connection(
                                        self._factory,
                                        ssl             = True,
                                        sock            = rawsock,
                                        server_hostname = req.host,
                                        **kwargs
                                    )

        return transport, proto


class AsyncHttp(object):
    class Response(object):
        def __init__(self, response, content):
            self.response = response
            self.content = content

        def __repr__(self):
            return self.response.__repr__()

        def __str__(self):
            return self.response.__str__()

        @property
        def status(self):
            return self.response.status

        def plist(self):
            import plistlib
            return plistlib.loads(self.content)

        def json(self, encoding = None):
            return dict2(json.loads(self.decode(encoding)))

        def text(self, encoding = None):
            return self.decode(encoding)

        def decode(self, encoding = None, **kwargs):
            return self.content.decode(encoding or self.response._get_encoding(), **kwargs)

    def __init__(self, *, loop = None, timeout = 30, cookie_class = http.cookies.BaseCookie):
        self.loop = loop or asyncio.get_event_loop()
        self.TCPConnector = aiohttp.connector.TCPConnector(verify_ssl = False, share_cookies = True, loop = self.loop)
        self.ProxyConnector = ProxyConnector('http://localhost:80', verify_ssl = False, share_cookies = True, loop = self.loop)

        self.TCPConnector.cookies = cookie_class()
        self.ProxyConnector.cookies = cookie_class()

        self.headers = {}
        self.timeout = timeout

        self.connector = self.TCPConnector

    def __del__(self):
        self.close()

    def close(self):
        self.TCPConnector.close()
        self.ProxyConnector.close()

    @property
    def cookies(self):
        return self.connector.cookies

    def SetHeaders(self, headers):
        self.headers = headers

    def SetCookies(self, cookies):
        if not cookies:
            self.connector.cookies.clear()
        else:
            self.connector.update_cookies(cookies)

    def SetProxy(self, host, port, login = None, password = None, encoding = 'latin1'):
        if self.connector is not self.ProxyConnector:
            self.connector = self.ProxyConnector
            self.SetCookies(self.TCPConnector.cookies)

        self.ProxyConnector.setProxy(host, port, login, password, encoding)

    def ClearProxy(self):
        if self.connector is not self.ProxyConnector:
            return

        self.connector = self.TCPConnector
        self.SetCookies(self.ProxyConnector.cookies)

    async def request(self, method, url, **kwargs):
        params = {}

        for key in ['noQuotoPath']:
            try:
                params[key] = kwargs.pop(key)
            except KeyError:
                pass

        kwargs['connector'] = self.connector
        kwargs['request_class'] = _ClientRequest.factory(**params)

        if 'headers' not in kwargs:
            kwargs['headers'] = self.headers
        else:
            hdr = self.headers.copy()
            for k, v in kwargs['headers'].items():
                hdr[k] = v

            kwargs['headers'] = hdr

        try:
            response = await asyncio.wait_for(aiohttp.request(method, url, **kwargs), self.timeout)
            content = await asyncio.wait_for(response.read(), self.timeout)
        except asyncio.TimeoutError:
            raise asyncio.TimeoutError('%s %s timeout: %s' % (method, url, self.timeout))

        return self.Response(response, content)
