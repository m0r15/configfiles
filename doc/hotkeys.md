# Горячие клавиши

## VIM
### Управление NERRD Tree

o - Открыть/закрыть каталог в дереве, открыть файл и перейти в окно с файлом
t - Открыть в новой вкладке
s - Разделить по вертикали и открыть
go - Открыть файл, но курсор остается на NERD Tree
i - открыть файл в отдельном окне, разделение идет по горизонтале
gi - тоже самое, но курсор остается на нерде
C - делает корнем текущую директорию
u - сделать корнем директорию выше текущей
cd - изменить CWD на директорию, на которую указывает курсор. То есть относительные пути в таких командах как :e :o теперь будут отличаться от этой директории
r - обновить директорию под курсором
R - обновить текущий корень дерева
m - показать меню
C-N v открываем нерд три
C-N x закрываем нерд

### NERDCommenter

leader=\
\cc - комментирует выделенную строку в визуальном режиме
\c space - коментировать/раскоментирвать выделенную строку

### Управление буферами

:bn следующий буфер
:bp предыдущий
:ls просмотреть открытые буферы
:b `имя_буфера` переключиться на буфер, очень удобно комбинируется с табом, к римеру пишем :b domain, жмём таб и нам подставляется открытый iis_domain.cpp
:bd удалить текущий буфер, правда стоит заметить, что если этот буфер единственное окно то vim закроется
:bd `имя_буфера` удалить буфер по имени

### Работа с окнами

Ctrl-w стрелочки :) — переместиться на окно влево/вправо/вверх/вниз
Сtrl-w o — развернуть окно
C-W n - создать новое окно
Ctrl-w c — закрыть
Ctrl-w s — разделить окно по горизонтали
Ctrl-w v — тоже, только по вертикали
Ctrl-w ] — разделить и перейти на определение чего-то, что под курсором
Ctrl-w f — разделить и в новом окне открыть файл путь к которому находится под курсором, очень удобно делать на инклюдах
C-W= - Подогнать окно по размеру
C-W+ - увеличить окно
С-Ц- - уменьшить окно
**Команды:**
:split — разделить, если указан файл то открыть его
:vsplit — тоже только по вертикали
:sb[uffer] — разделить и редактировать буффер. Важный момент: если заново открыть файл (к примеру через :split) то буфер сбрасывается, вместе с историей отмен и положением курсора

### Файл сессии

:mks nameoffile
vim -S nameoffile or :source nameoffile



## TMUX

C-a переводит в командный режим
C-a - горизонтальное разбиение
C-a | вертикальное изменнение
C-j предыдущее окно
C-k следующее окно
C-a C-x закрывает панель или окно
C-a C-c создает новое окно
C-a C-, переименовывает окно
C-a C-num переключает на окно номер num


## SHELL
### Moving the cursor:

Ctrl + a   Go to the beginning of the line (Home)
Ctrl + e   Go to the End of the line (End)
Ctrl + p   Previous command (Up arrow)
Ctrl + n   Next command (Down arrow)
Alt + b   Back (left) one word
Alt + f   Forward (right) one word
Ctrl + f   Forward one character
Ctrl + b   Backward one character
Ctrl + xx  Toggle between the start of line and current cursor position

### Editing:

Ctrl + L   Clear the Screen, similar to the clear command

Alt + Del  Delete the Word before the cursor.
Alt + d    Delete the Word after the cursor.
Ctrl + d   Delete character under the cursor
Ctrl + h   Delete character before the cursor (Backspace)

Ctrl + w   Cut the Word before the cursor to the clipboard.
Ctrl + k   Cut the Line after the cursor to the clipboard.
Ctrl + u   Cut/delete the Line before the cursor to the clipboard.

Alt + t    Swap current word with previous
Ctrl + t   Swap the last two characters before the cursor (typo).
Esc  + t   Swap the last two words before the cursor.

ctrl + y   Paste the last thing to be cut (yank)
Alt + u    UPPER capitalize every character from the cursor to the end of the current word.
Alt + l    Lower the case of every character from the cursor to the end of the current word.
Alt + c    Capitalize the character under the cursor and move to the end of the word.
Alt + r    Cancel the changes and put back the line as it was in the history (revert).
ctrl + _   Undo

TAB        Tab completion for file/directory names
           For example, to move to a directory 'sample1'; Type cd sam ; then press TAB and ENTER. 
           type just enough characters to uniquely identify the directory you wish to open.

### History:

Ctrl + r   Recall the last command including the specified character(s)       searches the command history as you type.
**Equivalent to : vim ~/.bash_history.**
Ctrl + p   Previous command in history (i.e. walk back through the command history)
Ctrl + n   Next command in history (i.e. walk forward through the command history)

Ctrl + s   Go back to the next most recent command. (beware to not execute it from a terminal because this will also launch its XOFF).
Ctrl + o   Execute the command found via Ctrl+r or Ctrl+s
Ctrl + g   Escape from history searching mode
      !!   Repeat last command
    !abc   Run last command starting with abc
  !abc:p   Print last command starting with abc
      !$   Last argument of previous command
 ALT + .   Last argument of previous command
      !*   All arguments of previous command
^abc­^­def Run previous command, replacing abc with def

### Process control:

Ctrl + C   Interrupt/Kill whatever you are running (SIGINT)
Ctrl + l   Clear the screen
Ctrl + s   Stop output to the screen (for long running verbose commands)
              Then use PgUp/PgDn for navigation
Ctrl + q   Allow output to the screen (if previously stopped using command above)
Ctrl + D   Send an EOF marker, unless disabled by an option, this will close the current shell (EXIT)
Ctrl + Z   Send the signal SIGTSTP to the current task, which suspends it.    To return to it later enter fg 'process name' (foreground).

***

## Tutorial for Emmet.vim
mattn <mattn.jp@gmail.com>

### 1. Expand an Abbreviation

  Type the abbreviation as 'div>p#foo$*3>a' and type '<c-y>,'.
  ```
  <div>
      <p id="foo1">
          <a href=""></a>
      </p>
      <p id="foo2">
          <a href=""></a>
      </p>
      <p id="foo3">
          <a href=""></a>
      </p>
  </div>
  ```

### 2. Wrap with an Abbreviation

  Write as below.
  ```
  test1
  test2
  test3
  ```
  Then do visual select(line wise) and type '<c-y>,'.
  Once you get to the 'Tag:' prompt, type 'ul>li*'.
  ```
  <ul>
      <li>test1</li>
      <li>test2</li>
      <li>test3</li>
  </ul>
  ```

  If you type a tag, such as 'blockquote', then you'll see the following:
  ```
  <blockquote>
      test1
      test2
      test3
  </blockquote>
  ```

### 3. Balance a Tag Inward

  type '<c-y>d' in insert mode.

### 4. Balance a Tag Outward

  type '<c-y>D' in insert mode.

### 5. Go to the Next Edit Point

  type '<c-y>n' in insert mode.

### 6. Go to the Previous Edit Point

  type '<c-y>N' in insert mode.

### 7. Update an <img>’s Size

  Move cursor to the img tag.
  ```
  <img src="foo.png" />
  ```
  Type '<c-y>i' on img tag
  ```
  <img src="foo.png" width="32" height="48" />
  ```

### 8. Merge Lines

  select the lines, which include '<li>'
  ```
  <ul>
    <li class="list1"></li>
    <li class="list2"></li>
    <li class="list3"></li>
  </ul>
  ```
  and then type '<c-y>m'
  ```
  <ul>
    <li class="list1"></li><li class="list2"></li><li class="list3"></li>
  </ul>
  ```

### 9. Remove a Tag

  Move cursor in block
  ```
  <div class="foo">
    <a>cursor is here</a>
  </div>
  ```
  Type '<c-y>k' in insert mode.
  ```
  <div class="foo">

  </div>
  ```

  And type '<c-y>k' in there again.
  ```

  ```

### 10. Split/Join Tag

  Move the cursor inside block
  ```
  <div class="foo">
    cursor is here
  </div>
  ```
  Type '<c-y>j' in insert mode.
  ```
  <div class="foo"/>
  ```

  And then type '<c-y>j' in there again.
  ```
  <div class="foo">
  </div>
  ```

### 11. Toggle Comment

  Move cursor inside the block
  ```
  <div>
    hello world
  </div>
  ```
  Type '<c-y>/' in insert mode.
  ```
  <!-- <div>
    hello world
  </div> -->
  ```
  Type '<c-y>/' in there again.
  ```
  <div>
    hello world
  </div>
  ```

### 12. Make an anchor from a URL

  Move cursor to URL
  ```
  http://www.google.com/
  ```
  Type '<c-y>a'
  ```
  <a href="http://www.google.com/">Google</a>
  ```

### 13. Make some quoted text from a URL

  Move cursor to the URL
  ```
  http://github.com/
  ```
  Type '<c-y>A'
  ```
  <blockquote class="quote">
    <a href="http://github.com/">Secure source code hosting and collaborative development - GitHub</a><br />
    <p>How does it work? Get up and running in seconds by forking a project, pushing an existing repository...</p>
    <cite>http://github.com/</cite>
  </blockquote>
  ```

### 14. Installing emmet.vim for the language you are using:

  ```
  # cd ~/.vim
  # unzip emmet-vim.zip
  ```
  Or if you are using pathogen.vim:
  ```
  # cd ~/.vim/bundle # or make directory
  # unzip /path/to/emmet-vim.zip
  ```
  Or if you get the sources from the repository:
  ```
  # cd ~/.vim/bundle # or make directory
  # git clone http://github.com/mattn/emmet-vim.git
  ```
### 15. Enable emmet.vim for the language you using.

  You can customize the behavior of the languages you are using.

  ```
  # cat >> ~/.vimrc
  let g:user_emmet_settings = {
  \  'php' : {
  \    'extends' : 'html',
  \    'filters' : 'c',
  \  },
  \  'xml' : {
  \    'extends' : 'html',
  \  },
  \  'haml' : {
  \    'extends' : 'html',
  \  },
  \}
  ```
