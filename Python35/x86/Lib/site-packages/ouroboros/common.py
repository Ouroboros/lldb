import os
import sys
import io
import json
import time
import struct
import shutil
import asyncio
import traceback
import configparser
import xml.etree.ElementTree as ET
import xmltodict

from ctypes import cdll, byref

if sys.platform == 'win32':
    from ctypes import windll
    from ctypes import wintypes
    from ctypes.wintypes import *

ANSI_CODE_PAGE = 'mbcs'
