from gi.repository import GLib

from recoll import recoll

import dbus
import dbus.service
import dbus.mainloop.glib

usage = """Usage:
python example-service.py &
python example-client.py
python example-async-client.py
python example-client.py --exit-service
"""

class DemoException(dbus.DBusException):
    _dbus_error_name = 'com.example.DemoException'

class SomeObject(dbus.service.Object):
    @dbus.service.method("com.example.SampleInterface",
                         in_signature='s', out_signature='a(ss)')
    def query(self, queryStr):
        print(str(queryStr))
        db = recoll.connect()
        queryObj = db.query()
        nres = queryObj.execute(queryStr)
        results = queryObj.fetchmany(20)
        rtn = []
        for doc in results:
            doc.url = str(doc.url).replace('file://','')
            print("%s %s" % (doc.url, doc.title))
            rtn.append((doc.url, doc.title))
        #return ["Recoll", " query", " with unique name",
                #session_bus.get_unique_name()]
        return rtn

    @dbus.service.method("com.example.SampleInterface",
                         in_signature='s', out_signature='as')
    def HelloWorld(self, hello_message):
        print(str(hello_message))
        return ["Hello", " from example-service.py", "with unique name",
                session_bus.get_unique_name()]

    @dbus.service.method("com.example.SampleInterface",
                         in_signature='', out_signature='')
    def RaiseException(self):
        raise DemoException('The RaiseException method does what you might '
                            'expect')

    @dbus.service.method("com.example.SampleInterface",
                         in_signature='', out_signature='(ss)')
    def GetTuple(self):
        return ("Hello Tuple", " from example-service.py")

    @dbus.service.method("com.example.SampleInterface",
                         in_signature='', out_signature='a{ss}')
    def GetDict(self):
        return {"first": "Hello Dict", "second": " from example-service.py"}

    @dbus.service.method("com.example.SampleInterface",
                         in_signature='', out_signature='')
    def Exit(self):
        mainloop.quit()


if __name__ == '__main__':
    dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

    session_bus = dbus.SessionBus()
    name = dbus.service.BusName("com.example.SampleService", session_bus)
    object = SomeObject(session_bus, '/SomeObject')

    mainloop = GLib.MainLoop()
    print("Running example service.")
    print(usage)
    mainloop.run()
