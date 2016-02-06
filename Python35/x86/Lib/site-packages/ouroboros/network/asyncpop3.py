from ..asynclib import *
import poplib
import http.cookies
import socket
import re
import ssl

error_proto = poplib.error_proto

__all__ = ['AsyncPop3', 'error_proto']


POP3_PORT     = poplib.POP3_PORT
POP3_SSL_PORT = poplib.POP3_SSL_PORT
CR            = poplib.CR
LF            = poplib.LF
CRLF          = poplib.CRLF
_MAXLINE      = poplib._MAXLINE


class AsyncPop3(asyncio.Protocol):

    """This class supports both the minimal and optional command sets.
    Arguments can be strings or integers (where appropriate)
    (e.g.: retr(1) and retr('1') both work equally well.

    Minimal Command Set:
            USER name               user(name)
            PASS string             pass_(string)
            STAT                    stat()
            LIST [msg]              list(msg = None)
            RETR msg                retr(msg)
            DELE msg                dele(msg)
            NOOP                    noop()
            RSET                    rset()
            QUIT                    quit()

    Optional Commands (some servers support these):
            RPOP name               rpop(name)
            APOP name digest        apop(name, digest)
            TOP msg n               top(msg, n)
            UIDL [msg]              uidl(msg = None)
            CAPA                    capa()
            STLS                    stls()

    Raises one exception: 'error_proto'.

    Instantiate with:
            POP3(hostname, port=110)

    NB:     the POP protocol locks the mailbox from user
            authorization until QUIT, so be sure to get in, suck
            the messages, and quit, each time you access the
            mailbox.

            POP is a line-based protocol, which means large mail
            messages consume lots of python cycles reading them
            line-by-line.

            If it's available on your mail server, use IMAP4
            instead, it doesn't suffer from the two problems
            above.
    """

    encoding = 'UTF-8'

    def __init__(self, host, port = POP3_PORT, timeout = socket._GLOBAL_DEFAULT_TIMEOUT, loop = None):
        super().__init__()

        self.host = host
        self.port = port
        self.timeout = timeout
        self._debugging = 0

        self.loop = loop or asyncio.get_event_loop()

        self.file = None
        self.buffer = bytearray()
        self.transport = None

    def __del__(self):
        self.close()

    async def init(self):
        await self.create_connection()
        self.welcome = await self._getresp()

    async def create_connection(self):
        await self.loop.create_connection(lambda : self, self.host, self.port)

    ##################################################
    # asyncio.Protocol interface
    ##################################################

    def connection_made(self, transport):
        self.transport = transport

    def data_received(self, data):
        self.buffer.extend(data)

    def eof_received(self):
        pass

    def connection_lost(self, exc):
        pass

    ##################################################
    # asyncio.Protocol interface end
    ##################################################

    # Internal: send one command to the server (through _putline())

    async def _putline(self, line):
        if self._debugging > 1:
            print('*put*', repr(line))

        self.transport.write(line + CRLF)

    # Internal: send one command to the server (through _putline())

    async def _putcmd(self, line):
        if self._debugging:
            print('*cmd*', repr(line))

        line = bytes(line, self.encoding)
        return (await self._putline(line))

    # Internal: return one line from the server, stripping CRLF.
    # This is where all the CPU time of this module is consumed.
    # Raise error_proto('-ERR EOF') if the connection is closed.

    async def _getline(self):
        index = self.buffer.find(b'\n')

        while index == -1:
            await asyncio.sleep(0.1)
            index = self.buffer.find(b'\n')

        line = self.buffer[:index + 1]
        self.buffer[:index + 1] = b''

        if len(line) > _MAXLINE:
            raise error_proto('line too long')

        if self._debugging > 1:
            print('*get*', repr(line))

        if not line:
            raise error_proto('-ERR EOF')

        octets = len(line)
        # server can send any combination of CR & LF
        # however, 'readline()' returns lines ending in LF
        # so only possibilities are ...LF, ...CRLF, CR...LF
        if line[-2:] == CRLF:
            return line[:-2], octets
        if line[0] == CR:
            return line[1:-1], octets
        return line[:-1], octets


    # Internal: get a response from the server.
    # Raise 'error_proto' if the response doesn't start with '+'.

    async def _getresp(self):
        resp, o = await self._getline()

        if self._debugging > 1:
            print('*resp*', repr(resp))

        if not resp.startswith(b'+'):
            raise error_proto(resp)

        return resp


    # Internal: get a response plus following text from the server.

    async def _getlongresp(self):
        resp = await self._getresp()
        list = []; octets = 0
        line, o = await self._getline()
        while line != b'.':
            if line.startswith(b'..'):
                o = o-1
                line = line[1:]
            octets = octets + o
            list.append(line)
            line, o = await self._getline()
        return resp, list, octets


    # Internal: send a command and get the response

    async def _shortcmd(self, line):
        await self._putcmd(line)
        return (await self._getresp())


    # Internal: send a command and get the response plus following text

    async def _longcmd(self, line):
        await self._putcmd(line)
        return (await self._getlongresp())


    # These can be useful:

    def getwelcome(self):
        return self.welcome


    def set_debuglevel(self, level):
        self._debugging = level


    # Here are all the POP commands:

    async def user(self, user):
        """Send user name, return response

        (should indicate password required).
        """
        return (await self._shortcmd('USER %s' % user))


    async def pass_(self, pswd):
        """Send password, return response

        (response includes message count, mailbox size).

        NB: mailbox is locked by server from here to 'quit()'
        """
        return (await self._shortcmd('PASS %s' % pswd))


    async def stat(self):
        """Get mailbox status.

        Result is tuple of 2 ints (message count, mailbox size)
        """
        retval = await self._shortcmd('STAT')
        rets = retval.split()
        if self._debugging: print('*stat*', repr(rets))
        numMessages = int(rets[1])
        sizeMessages = int(rets[2])
        return (numMessages, sizeMessages)


    async def list(self, which=None):
        """Request listing, return result.

        Result without a message number argument is in form
        ['response', ['mesg_num octets', ...], octets].

        Result when a message number argument is given is a
        single response: the "scan listing" for that message.
        """
        if which is not None:
            return (await self._shortcmd('LIST %s' % which))
        return (await self._longcmd('LIST'))


    async def retr(self, which):
        """Retrieve whole message number 'which'.

        Result is in form ['response', ['line', ...], octets].
        """
        return (await self._longcmd('RETR %s' % which))


    async def dele(self, which):
        """Delete message number 'which'.

        Result is 'response'.
        """
        return (await self._shortcmd('DELE %s' % which))


    async def noop(self):
        """Does nothing.

        One supposes the response indicates the server is alive.
        """
        return (await self._shortcmd('NOOP'))


    async def rset(self):
        """Unmark all messages marked for deletion."""
        return (await self._shortcmd('RSET'))


    async def quit(self):
        """Signoff: commit changes on server, unlock mailbox, close connection."""
        resp = await self._shortcmd('QUIT')
        self.close()
        return resp

    def close(self):
        self.transport and self.transport.close()

    # __del__ = quit


    # optional commands:

    async def rpop(self, user):
        """Not sure what this does."""
        return (await self._shortcmd('RPOP %s' % user))


    timestamp = re.compile(br'\+OK.*(<[^>]+>)')

    async def apop(self, user, password):
        """Authorisation

        - only possible if server has supplied a timestamp in initial greeting.

        Args:
                user     - mailbox user;
                password - mailbox password.

        NB: mailbox is locked by server from here to 'quit()'
        """
        secret = bytes(password, self.encoding)
        m = self.timestamp.match(self.welcome)
        if not m:
            raise error_proto('-ERR APOP not supported by server')
        import hashlib
        digest = m.group(1)+secret
        digest = hashlib.md5(digest).hexdigest()
        return (await self._shortcmd('APOP %s %s' % (user, digest)))


    async def top(self, which, howmuch):
        """Retrieve message header of message number 'which'
        and first 'howmuch' lines of message body.

        Result is in form ['response', ['line', ...], octets].
        """
        return (await self._longcmd('TOP %s %s' % (which, howmuch)))


    async def uidl(self, which=None):
        """Return message digest (unique id) list.

        If 'which', result contains unique id for that message
        in the form 'response mesgnum uid', otherwise result is
        the list ['response', ['mesgnum uid', ...], octets]
        """
        if which is not None:
            return (await self._shortcmd('UIDL %s' % which))
        return (await self._longcmd('UIDL'))


    async def capa(self):
        """Return server capabilities (RFC 2449) as a dictionary
        >>> c=poplib.POP3('localhost')
        >>> c.capa()
        {'IMPLEMENTATION': ['Cyrus', 'POP3', 'server', 'v2.2.12'],
         'TOP': [], 'LOGIN-DELAY': ['0'], 'AUTH-RESP-CODE': [],
         'EXPIRE': ['NEVER'], 'USER': [], 'STLS': [], 'PIPELINING': [],
         'UIDL': [], 'RESP-CODES': []}
        >>>

        Really, according to RFC 2449, the cyrus folks should avoid
        having the implementation split into multiple arguments...
        """
        def _parsecap(line):
            lst = line.decode('ascii').split()
            return lst[0], lst[1:]

        caps = {}
        try:
            resp = await self._longcmd('CAPA')
            rawcaps = resp[1]
            for capline in rawcaps:
                capnm, capargs = _parsecap(capline)
                caps[capnm] = capargs
        except error_proto as _err:
            raise error_proto('-ERR CAPA not supported by server')
        return caps


    async def stls(self, context=None):
        """Start a TLS session on the active connection as specified in RFC 2595.

                context - a ssl.SSLContext
        """
        if not HAVE_SSL:
            raise error_proto('-ERR TLS support missing')

        if self._tls_established:
            raise error_proto('-ERR TLS session already established')

        caps = self.capa()

        if not 'STLS' in caps:
            raise error_proto('-ERR STLS not supported by server')

        if context is None:
            context = ssl._create_stdlib_context()

        resp = await self._shortcmd('STLS')
        self.sock = context.wrap_socket(self.sock, server_hostname = self.host)
        self._tls_established = True
        return resp

if poplib.HAVE_SSL:

    class AsyncPop3SSL(AsyncPop3):
        """POP3 client class over SSL connection

        Instantiate with: POP3_SSL(hostname, port=995, keyfile=None, certfile=None,
                                   context=None)

               hostname - the hostname of the pop3 over ssl server
               port - port number

        See the methods of the parent class POP3 for more documentation.
        """

        def __init__(
                self,
                host,
                port    = POP3_SSL_PORT,
                timeout = socket._GLOBAL_DEFAULT_TIMEOUT,
                loop    = None
            ):

            super().__init__(host, port, timeout, loop)

        async def create_connection(self):
            await self.loop.create_connection(lambda : self, self.host, self.port, ssl = ssl._create_stdlib_context())

        def stls(self, *args, **kwargs):
            """The method unconditionally raises an exception since the
            STLS command doesn't make any sense on an already established
            SSL/TLS session.
            """
            raise error_proto('-ERR TLS session already established')

    __all__.append('AsyncPop3SSL')
