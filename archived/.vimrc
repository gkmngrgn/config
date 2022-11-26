"" Gökmen Görgen's vim & gvim configurations
" I'm using vim with: taglist, snipmate, colorsamplerpack, railscasts theme

syntax on
set encoding=utf-8
set bg=dark
set nocompatible
set showcmd
set hlsearch
set incsearch
set novisualbell
set title
set ttyfast
set modeline
set is
set ic
set ls=2
set ts=4
set sts=4
set sw=4
set listchars=tab:·\ ,trail:·
set list
set autoindent
set laststatus=2
set expandtab
set statusline=%<\ %n:%f\ %m%r%y%=%-35.(line:\ %l\ of\ %L,\ col:\ %c%V\ (%P)%)
set clipboard=autoselect,unnamed,exclude:cons\|linux
set wildmode=longest,list

if has("unix")
    set autochdir
    set shell=/bin/bash
endif

"" Remove Unwanted White Spaces
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

"" Gui Options
if has("gui_running")
    set guioptions-=r
    set guioptions-=m
    set guioptions-=T
    set lines=30
    set columns=120
    if has("macunix")
        set guifont=Monaco:h12
        set antialias
        colorscheme murphy
    else
        set guifont=Droid\ Sans\ Mono\ 11
        colorscheme railscasts
    endif
endif

"" Text File Settings
map <F7> :set wrap<CR>:set textwidth=80<CR>
map <F8> :set nowrap<CR>:set textwidth=0<CR>

"" Remember Last Line
set viminfo='10,\"100,:20,%,n~/.viminfo
    au BufReadPost * if line("'\"") > 0|if line("'\"") <= line("$")|exe("norm '\"")|else|exe "norm
    $"|endif|endif

"" Taglist
let Tlist_Use_Right_Window=1
let Tlist_WinWidth=40
let Tlist_Exit_OnlyWindow=1
let Tlist_File_Fold_Auto_Close=1
nmap <C-T> :Tlist <CR>
