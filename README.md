# notifications-tray-icon
![Build Status](https://github.com/IvanMalison/notifications-tray-icon/actions/workflows/build.yml/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/notifications-tray-icon.svg?logo=haskell&label=notifications-tray-icon)](https://hackage.haskell.org/package/notifications-tray-icon) [![Stackage LTS](http://stackage.org/package/notifications-tray-icon/badge/lts)](http://stackage.org/lts/package/notifications-tray-icon) [![Stackage Nightly](http://stackage.org/package/notifications-tray-icon/badge/nightly)](http://stackage.org/nightly/package/notifications-tray-icon)

notifications-tray-icon provides an SNI tray icon that polls a notification
source and provides a menu with actions from that notification source. It will
add an overlay to the displayed icon when new notifications are present, and
call notify-send to display a notification when new notifications come in.

# Sources

For the time being, github is the only supported notification source. Support
for gmail and other sources should be coming soon.

# Installation

For the time being, notifications-tray-icon must be installed from source using
stack.
