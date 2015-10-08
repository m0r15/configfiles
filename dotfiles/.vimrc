"------------------------------------------------------------------------------
" VundleBundle plugin manager autoinstall
"   Brief help
"   :PluginList         - lists configured plugins
"   :PluginInstall      - installs plugins; append '!' to update or just
"                           :PluginUpdate
"   :PluginSearch       - searchs for foo; append '!' to refresh local cache
"   :PluginClean        - confirms removal of unused plugins; append '!' to
"                           auto-approve r
"   See :help vundle for take help about vundle bundle plugin

" Turn off file type plugins before bundles init
set nocompatible
filetype off
" Setting up Vundle - the vim plugin bundler
    let iCanHazVundle=1
    let vundle_readme=expand('~/.vim/bundle/Vundle.vim/README.md')
    if !filereadable(vundle_readme)
        echo "Installing vundle..."
        echo ""
        silent !mkdir -p ~/.vim/bundle
        silent !git clone https://github.com/VundleVim/Vundle.vim ~/.vim/bundle/Vundle.vim
        let iCanHazVundle=0
    endif
    set rtp+=~/.vim/bundle/Vundle.vim
    "call vundle#begin()
    "plugin "VundleVim/Vundle.vim"

    "... All your other bundles...
    if iCanHazVundle == 0
        echo "Installing Bundles, please ignore key map error message"
        echo ""
        :PluginInstall
    endif
" Setting up Vundle - the vim plugin bundler end

" Plugins
call vundle#begin()

"Vundle
Plugin 'VundleVim/Vundle.vim'

" Airlines
Plugin 'bling/vim-airline'

" vim-bufferline
Plugin 'bling/vim-bufferline'

" minibufexpl
Plugin 'weynhamz/vim-plugin-minibufexpl'

" Add code static check on write need to be properly configured
Plugin 'scrooloose/syntastic'

" Greate file manager
Plugin 'scrooloose/nerdtree'

" Pending tasks list
Plugin 'TaskList.vim'

" Base 16 VIM
Plugin 'chriskempson/base16-vim'

" Surround
Plugin 'tpope/vim-surround'

" GitGutter
Plugin 'airblade/vim-gitgutter'

" vim-signify
Plugin 'mhinz/vim-signify'

call vundle#end()
" Enable Indent in plugins
filetype plugin indent on

"------------------------------------------------------------------------------
" Bundle config

"------------------------------------------------------------------------------
" vim-airline

" Colorscheme for airline
"let g:airline_theme='understated'
let g:airline_theme='base16'

" Set custom left separator
"let g:airline_left_sep='▶'

" Set custom right separator
"let g:airline_right_sep = '◀'

" Enable airline for tab-bar
let g:airline#extensions#tabline#enabled = 1

" Don't display buffers in tab-bar with single tab
let g:airline#extensions#tabline#show_buffers = 0

" Display only filename in tab
let g:airline#extensions#tabline#fnamemod = ':t'

" Don't display encoding
"let g:airline_section_y = ''

" Don't display filetype
"let g:airline_section_x = ''

"------------------------------------------------------------------------------
" Syntastic

" Enable autockeck
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1

" For correct works of next/previous error navigation
let g:syntastic_always_populate_loc_list=1

" open quicfix window with all error found
nmap <silent> <leader>ll :Errors<cr>
" previous syntastic error
nmap <silent> [ :lprev<cr>
" next syntastic error
nmap <silent> ] :lnext<cr>

"------------------------------------------------------------------------------
" NERDTree

" Tell NERDTree to display hidden files on startup
let NERDTreeShowHidden=1

" Disable bookmarks label, and hint about '?'
let NERDTreeMinimalUI=1

" Display current file in the NERDTree on the left
nmap <silent> <leader>f :NERDTreeFind<CR>

" Display NERDTree
map <F3> :NERDTreeToggle<CR>

"------------------------------------------------------------------------------
" TaskList

" hot key List of tasks
map <F2> :TaskList<CR>


"------------------------------------------------------------------------------
" Colorscheme

" Enable syntax colorise
syntax on

"colorscheme desert
colorscheme base16-default
" Add 256 color terminal
let base16colorspace=256

" Setting up dark scheme
set background=dark

" set highlight for column
highlight ColorColumn ctermbg=darkgray

"------------------------------------------------------------------------------
" General options

" Auto reload changed files
set autoread

" Indicate fast terminal connection
set ttyfast

" Set character encoding
set encoding=utf-8

" Let vim know what encoding we use in our terminal
set termencoding=utf-8

" Use 256 colors in vim
" vim-airline not work without it
set t_Co=256

" Use system buffer
set clipboard=unnamed

" Which EOl used. For us it's unix
" Not works with modifiable=no
if &modifiable
    set fileformat=unix
endif
"------------------------------------------------------------------------------
" Display options

" Show or hide modification word. For example is -- INSERT --
" if showmode set to hide this word do not display
"set showmode
set noshowmode

" Show file name in window title
set title

" Mute error bell
set novisualbell

" Remove all useless messages like intro screen and usee abbreviation like RO
" instead readonly and + instead modified
set shortmess=atI

" Enable display whitespace character
set list

" Setting up how to display whitespace characters
set listchars=tab:⇥\ ,trail:·,extends:⋯,precedes:⋯,nbsp:~

" Number of rows to keep to the left and to the  right off the scree
set scrolloff=5

" Numbers of columns to keep to the left and to the right off the screen
set sidescrolloff=5

" Vim will move to the previous/next line after reaching first/last char in
" the line with this commnad (you can add 'h' or 'l' here as well)
" <,> stand for arrows in command mode and [,] arrows in visual mode
set whichwrap=b,s,<,>,[,],

" Display comand which you typing and jther command related stuff
set showcmd

" Indicate that last window have a statusline too
set laststatus=2

" Add a line / column display in the bottom right-hand section of the screen.
" Not needed with airline plugin
set ruler

" Setting up right-hand section(ruller) format
" Not needed with airline plugin
set rulerformat=%30(%=\:%y%m%r%w\ %l,%c%V\ %P%)

" The cursor should stay where you leave it, instead of moving to  the first
" non blank of the line
set nostartofline

" Disable wrapping long string
set nowrap

" Display line numbers
set number

" Highlight line with cursor
"if has("gui_running")
"    set cursorline
"endif
set cursorline

" Hide panel in GUI
set guioptions-=m
set guioptions-=T
set guioptions-=r

" maximum text length at 80 symbols vim automatically breaks longer lines
set textwidth=80

" Hightlight column right after max textwidth
set colorcolumn=+1

" Menu by mouse
"aunmenu Help.
"aunmenu Window.
let no_buffers_menu=1
set mousemodel=popup

" Dialog for confirmations
"set confirm

" Display .h file as C, as not CPP
let c_syntax_for_h=""

" Save indent for paste
set pastetoggle=
"------------------------------------------------------------------------------
" Tab options

" Copy indent from previous line
set autoindent

" Indent like C-style
set cindent

" Enable smart indent, it add additional indents
set smartindent

" Replace tabs with spaces
set expandtab

" When you hit tab at start of line, indent added according to shiftwidth value
set smarttab

" number of spaces to use for each step of indent
set shiftwidth=4

" Number of spaces to use a Tab in the file counts for
set tabstop=4

" Same but for editing operation (not shure what exactly does it means)
" but in most cases tabstop and softtabstop better be the same
set softtabstop=4

" Round indent to multiple of 'shiftwidth'.
" Indentation always be multiple of shiftwidth
" Applies to  < and > command
set shiftround

"------------------------------------------------------------------------------
" Search options

" Highlight search results
set nohlsearch

" Ignore case in search patterns
set ignorecase

" Override the 'ignorecase' option if the search patter ncontains upper case
" characters
set smartcase

" Live search. While typing a search command, show where the pattern
set incsearch

" Show matching brackets
set showmatch

" Make < and > match as well
set matchpairs+=<:>

"------------------------------------------------------------------------------
" Edit

" Allow backspace to remove indents, newlines and old text
set backspace=indent,eol,start

" Disable backup file
set nobackup

" Disable vim common sequense for saving.
" By defalut vim write buffer to a new file, then delete original file
" then rename the new file.
set nowritebackup

" Disable swap files
set noswapfile

" Do not add eol at the end of file
"set noeol

"------------------------------------------------------------------------------
" Diff options

"Disable filler
set diffopt=filler

" Open diff in horizontal mode
set diffopt+=horizontal

" Ignore changes in whitespaces characters
set diffopt+=iwhite

"------------------------------------------------------------------------------
" Autocmd

" It execute specific command when specific events occured
" like reading or writing file, or open close buffer
if has("autocmd")
    " Define group of commands,
    " Commands defined in .vimrc don't bind twice if .vimrc will reload
    augroup vimrc
    " Delete any previosly defined autocommands
    au!
        " Auto reload vim after your change it
        au BufWritePost *.vim source $MYVIMRC | AirlineRefresh
        au BufWritePost .vimrc source $MYVIMRC | AirlineRefresh

        " Restore cursor position
        au BufReadPost * if line("'\"")>0 && line("'\"")<=line("$")
                    \| exe "normal g'\"" | endif

        " For this type of file tab change to
        au FileType crontab,fstab,make set noexpandtab tabstop=8 shiftwidth=8

        " --- Pythom ---
        "au FileType python set completeopt-=preview
        "au FileType python set expandtab shiftwidth=4 tabstop=8
        "\ formatoptions+=croq softtabstop=4 smartindent
        "\ cinwords=if,elif,else,for,while,try,except,finally,def,class, with
"        au FileType pyrex

        " --- Rust ---
        "au BufNewFile,BufRead *.rs set filetype=rust

    "Group end
    augroup END
endif

"------------------------------------------------------------------------------
" Hotkeys

" C-s is Quicksave command
noremap <C-s> :update<CR>
vnoremap <C-s> <C-C>:update<CR>
inoremap <C-s> <C-O>:update<CR>

" C-o is Quick open file
noremap <C-o> :browse confirm e<CR>
vnoremap <C-o> :browse confirm e<CR>
inoremap <C-o> :brows confirm e<CR>

" Activare auto complete
inoremap <C-space> <C-x><C-o>

" Easy switching language
"nnoremap <leader>Th :set ft=

