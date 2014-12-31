#!/bin/sh

# put the path to your Inbox folder here
CHECKDIR="/home/$LOGNAME/Maildir/INBOX"

# echo "\ncalled" >> /home/greg/called

sauronmsg () {
    DBUS_COOKIE="/home/$LOGNAME/.sauron-dbus"
    if test "x$DBUS_SESSION_BUS_ADDRESS" = "x"; then
#				echo "\ntestxdbussessionbusaddressx" >> /home/greg/called
        if test -e $DBUS_COOKIE; then
#								echo "\testdbuscookie" >> /home/greg/called
                export DBUS_SESSION_BUS_ADDRESS="`cat $DBUS_COOKIE`"
        fi
    fi
    if test -n "x$DBUS_SESSION_BUS_ADDRESS"; then
#				echo "\n" >> /home/greg/called
#				echo $DBUS_SESSION_BUS_ADDRESS >> /home/greg/called
        dbus-send --session                          \
            --dest="org.gnu.Emacs"                   \
            --type=method_call                       \
            "/org/gnu/Emacs/Sauron"                  \
            "org.gnu.Emacs.Sauron.AddMsgEvent"       \
            string:Mail uint32:3 string:"$1"
    fi
}

#
# -mmin -5: consider only messages that were created / changed in the
# the last 5 minutes
#
for f in `find $CHECKDIR -mmin -5 -a -type f`; do
				subject=`mu view $f | grep '^Subject:' | sed 's/^Subject://'`
				from=`mu view $f | grep '^From:' | sed 's/^From://'`
        sauronmsg "$subject | $from"
				echo  "New Mail: $subject | $from" >> /home/greg/called
done
