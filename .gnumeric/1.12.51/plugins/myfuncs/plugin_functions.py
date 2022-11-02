#!/usr/bin/env python3

from hashlib import md5

# from Gnumeric import GnumericError, GnumericErrorVALUE
# import Gnumeric
# import string

def func_md5(i):
    try:
        ret = md5(str(i).encode()).hexdigest()
    except:
        ret = md5(str(i)).hexdigest()
    return ret

austinjp_functions = {
    "py_md5": func_md5
}


