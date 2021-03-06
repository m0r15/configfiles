Помощь по Git
=============
Модель ветвление для Git
------------------------
### Предисловие ###

Вольный пересказ вот [этой вот](http://habrahabr.ru/post/106912/) статьи. Так сказать все что нужно для реальной работы. Для себя я выбрал именно эту модель.

### Главные ветви ###

Ядро модели состоит из 2х веток

* master
* develop
    
Ветвь `master` при инициализации репозитория. Допустим, репозитарий создан и склонирован на диск. Создадим `develop` ветку и перейдем на

    $ git checkout -b develop master
    $ git push origin develop:develop  # Создаст на сервере
    $ git pull origin develop  # Установит связь с веткой

Ветвь `origin/master` считается главной. Исходный код в ней должен находиться в состоянии *production-ready* в любой произвольный момент времени.
Ветвь `origin/develop` считаем главной веткой для разработки. Внем у нас последние изменения.

Когда исходный код в `devrlop` достигает стабильного состояния и готов к релизу, все изменения должны быть определенным образом влиты в `master` и помечены тегом с номером релиза.


### Вспомогательные ветви ###

  Помимо `master` и `develop`, модель разработки содержит некоторое количество типов вспомогательных ветвей, которые используются для распараллеливания разработки.

Типы ветвей:
* Ветви функциональностей (*Feature branches*)
* Ветви релизов (*Realease branches*)
* Ветви исправлений (*Hotfix branchese*)

### Ветви функциональностей ###

**Вливаются:** `develop`
**Наименование:** все, за исключением *master*, *delop*, *release-*, *hotfix-*

  Используются для разработки новых функций, которые должны появится в текущем или будущих релизах. При начале работы над функциональностью. Время жизни == столько сколько идет работа над функциональностью.

  Обычно существуют в репозитариях разработчиков, но не в главном репозитарии (`origin`).

#### Создание ветви функциональности ####

    $ git checkout -b myfeature develop
    (Если все таки нужно добавить в origin)
    $ git push origin myfeature:myfeature
    $ git pull origin myfeature
    (end)

#### Добавление завершенной функциональности ####

    $ git checkout develop
    $ git merge --no-ff myfeature
    $ git branch -d myfeature
    (если добавляли в origin)
    $ git push origin :myfeature
    (end)
    $ git push origin develop

  Флаг `--no-ff` вынуждает Git всегда создавать новый объект коммита при слиянии. Это позволяет не терять информацию о том, что ветка существовала.

### Ветви релизов ###

**Пораждаются:** *develop*
**Вливаются:** *develop* и *master*
**Именование:** *release-*

  Используются для подготовки к выпуску новых версий продукта. 

#### Создание ветви релиза ####

    $ git checkout -b release-0.0.1 develop
    $ cat 0.0.1 > VERSION
    $ git commit -a -m "Change version number to 0.0.1"

#### Закрытие ветви релиза ####

    $ git checkout master
    $ git merge --no-ff release-0.0.1
    $ git tag -a 0.0.1

**Замечание:** при желании, можно использовать флаги -s и -u <key>, что бы криптографически подписать тэг.

    $ git checkout develop
    $ git merge --no-ff release-0.0.1

    $ git branch -d release-0.0.1

### Ветви исправлений ###

**Могут пораждаться от:** *master*
**Должны вливаться в:** *develop* и *master*
**Соглашение о именовании:** *hotfix-*

  Используется для бысрых исправлений. Ветви исправлений создаются из главной ветви. если у нас релиз 1.2 то исправления 1.2.1

    $ git checkout -b hotfix-1.2.1 master
    $ echo 1.2.1 > VERSION
    $ git commit -a -m "Change version to 1.2.1"

  Правим баг.

    $ git commit -m "Fixed"
    $ git checkout master
    $ git merge --no-ff hotfix-1.2.1
    $ git tag -a 1.2.1

    $ git checkout develop
    $ git merge --no-ff hotfix-1.2.1

  *Если в данный момент существует ветвь релиза, то ветвь хотфикса должна вливаться в релиз*

    $ git branch -d hotfix-1.2.1

---
## Нбольшой список команд, который поможет в дальнейшем
* `git clone [путь до репозитария]` - склонировать репу
* `git pull --ff-only origin master` - Если репа устаревшая, выполняется только в случае, когда обновление будет "прямой перемоткой"
* `git log --oneline --graph --decorate` - покакет симпатичную ветку
* `git log --follow <path>` - лог изменений, который коснулся только конкретного файла
* `git merge --abort` - отменяет слияение, если что-то пошло не так
* `git reset --hard` - отменяет все не зафиксированные изменения. Но НЕ ИСПОЛЬЗОВАТЬ НЕ ОБДУМАННО
* `git rebase --abort` - гарантировано вернет туда откуда начал.
* `git reflog` - если потерял коммиты, то это может их вернуть
* 

---
## Помощь от Github ##
create a new repository on the command line
```bash
echo "# photobooth" >> README.md
git init
git add README.md
git commit -m "first commit"
git remote add origin https://github.com/m0r15/photobooth.git
git push -u origin master
```

push an existing repository from the command line
```bash
git remote add origin https://github.com/m0r15/photobooth.git
git push -u origin master
```
