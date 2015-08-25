# Установка EVE на Ubuntu 14.04

    NOTE: Мой скрипт e1 запускает eve с лаунчером а v1 без. Лаунчер очень тормозит и вообще плохо работает с линукс, но в нем доступны обновления. Связаны проблемы с неправильной работой SSL в Wine

1. Устанавливаем Ubuntu 14.04
2. Настраиваем видеодрайвер
3. Устанавливаем Wine. apt-get install install wine (оно должно быть версии 1.6) также можно доустановить "winetricks"
4. Устанавливаем EVE. Качаем офлайн инсталлер ( http://community.eveonline.com/support/download/offline-installer/). Переходим в директорию, куда скачали, запускаем с помощью wine инсталлер (возможно придется пофиксить права доступа)
5. Указываем путь куда устанавливать игру (я выбрал C:\Games\CCP\EVE)
6. Устанавливаем следующие скрипты
    ~/bin/v1
    ```
    #!/bin/bash
    /usr/bin/env WINEPREFIX="$HOME/wine" DISPLAY=":0.0" wine explorer/desktop=eve1,1024x768 "C:\Games\CCP\EVE\bin\exefile.exe"
    ```
    Этот скрипт использовать если мы хотим загрузить обновления ~/bin/e1
    ```
    #!/bin/bash
    /usr/bin/env WINEPREFIX="$HOME/wine" DISPLAY=":0.0" wine explorer/desktop=eve1,1024x768 "C:\Games\CCP\EVE\eve.exe"
    ```