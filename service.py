from gi.repository import GLib

from recoll import recoll

import dbus
import dbus.service
import dbus.mainloop.glib

usage = """Usage:
python service.py &
"""

class DemoException(dbus.DBusException):
    _dbus_error_name = 'com.sfs.QueryException'

class SFS(dbus.service.Object):
    @dbus.service.method("com.sfs.SearchInterface",
                         in_signature='s', out_signature='a(ss)')
    def Query(self, queryStr):
        # print(str(queryStr))
        db = recoll.connect()
        queryObj = db.query()
        nres = queryObj.execute(queryStr)
        results = queryObj.fetchmany(500)
        rtn = []
        for doc in results:
            doc.url = str(doc.url).replace('file://','')
            print("%s %s" % (doc.url, doc.title))
            rtn.append((doc.url, doc.title))
        return rtn

    @dbus.service.method("com.sfs.SearchInterface",
                         in_signature='', out_signature='')
    def RaiseException(self):
        raise QueryException('The RaiseException method does what you might '
                            'expect')

    @dbus.service.method("com.sfs.SearchInterface",
                         in_signature='', out_signature='')
    def Exit(self):
        mainloop.quit()


if __name__ == '__main__':
    dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

    session_bus = dbus.SessionBus()
    name = dbus.service.BusName("com.sfs.SearchService", session_bus)
    object = SFS(session_bus, '/SFS')

    mainloop = GLib.MainLoop()
    print("Running SFS service.")
    print(usage)
    mainloop.run()
