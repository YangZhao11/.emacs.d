(use-package ibuffer-sidebar
  :bind ("M-s b" . z-ibuffer-sidebar)
  :config
  (setq ibuffer-sidebar-width 30)
  (defun z-ibuffer-sidebar ()
    (interactive)
    (imenu-list-minor-mode -1)
    (ibuffer-sidebar-toggle-sidebar)))

(use-package dired-sidebar
  :bind ("M-s j" . z-dired-sidebar)
  :config
  (setq dired-sidebar-theme 'nerd
        dired-sidebar-width 30)
  (defun z-dired-sidebar ()
    (interactive)
    (imenu-list-minor-mode -1)
    (dired-sidebar-toggle-sidebar)))

;; TODO: hydra for table commands. Ref. "Text Based Tables" in emacs manual.
(use-package table
  :bind ("M-m" . hydra-table/body)
  :config
  (defhydra hydra-table (:color blue :hint nil)
    "
→ TAB    ca_p_ture  _u_n/reco_g_nize   _h_eighten  _w_iden    jus_t_ify   _i_nsert _r_ow/_c_ol   _0_: span
← S-TAB  re_l_ease  _U_n/reco_G_ cell  _s_horten   _n_arrow   se_q_uence  ^^delete _R_ow/_C_ol   _2__3_:split
"
    ("SPC" nil :exit t)
    ("p" table-capture)
    ("l" table-release)
    ("u" table-unrecognize-dwim)
    ("g" table-recognize-dwim)
    ("U" table-unrecognize-cell)
    ("G" table-recognize-cell)
    ("h" table-heighten-cell :exit nil)
    ("s" table-shorten-cell :exit nil)
    ("w" table-widen-cell :exit nil)
    ("n" table-narrow-cell :exit nil)
    ("t" table-justify)
    ("i" table-insert)
    ("q" table-insert-sequence)
    ("r" table-insert-row)
    ("c" table-insert-column)
    ("R" table-delete-row)
    ("C" table-delete-column)
    ("0" table-span-cell)
    ("2" table-split-cell-vertically)
    ("3" table-split-cell-horizontally))

  (defun table-recognize-dwim ()
    (interactive)
    (call-interactively
     (if (use-region-p) 'table-recognize-region
       'table-recognize-table)))
  (defun table-unrecognize-dwim ()
    (interactive)
    (call-interactively
     (if (use-region-p) 'table-unrecognize-region
       'table-unrecognize-table)))
)



(use-package which-key :ensure :diminish which-key-mode
  :bind ("C-x t /" . which-key-mode)
  :config (setq which-key-idle-delay 2)
  (setq which-key-key-replacement-alist
        (append '(("TAB" . "↹") ("DEL" . "⇤")("RET" . "⏎")("SPC" . "␣"))
                which-key-key-replacement-alist)))


(defun z-LaTeX-mode-hook ()
  (define-key LaTeX-mode-map (kbd "C-x `") 'next-error)
  (define-key LaTeX-mode-map (kbd "C-x <f1>") 'TeX-ispell-document)
  (turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        TeX-auto-save t
        TeX-parse-self t))
(add-hook 'LaTeX-mode-hook 'z-LaTeX-mode-hook)

(setq preferred-debugger-alist
      '((c-mode . gdb)
        (c++-mode . gdb)
        (cperl-mode . perldb)
        (python-mode . pdb)
        (jde-mode . jdb)))
(defun z-maybe-gud ()
  "Run gdb if not already running, otherwise bring it to front"
  (interactive)
  (require 'gud)
  (if (and (boundp 'gud-comint-buffer)  ;find running gdb process
           gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer))
      (if (fboundp 'gdb-restore-windows)
           (gdb-restore-windows)
        (pop-to-buffer gud-comint-buffer))
    (call-interactively
     (or (cdr (assq major-mode preferred-debugger-alist))
         'gdb))))
(global-set-key (kbd "C-x <f8>") 'z-maybe-gud)

(defun z-maybe-recompile (&optional arg)
  "recompile if possible"
  (interactive "P")
  (if (and (fboundp 'recompile) (not arg))
      (recompile)
    (call-interactively 'compile)))
(global-set-key [f5] 'z-maybe-recompile)

(require 'hydra)


;; --------------------------------------------------
;; eshell
(use-package eshell
  :commands eshell
  :functions eshell/pwd
  :config
  (defun z-eshell-prompt-function ()
    (let* ((pwd (eshell/pwd))
           (short-pwd (abbreviate-file-name pwd))
           (pwd-face 'default))
      (setq pwd-face
            (cond ((string= (nth 2 (file-attributes pwd 'string)) user-login-name)
                   'eshell-ls-symlink)
                  ((file-writable-p pwd) 'font-lock-constant-face)
                  ((file-readable-p pwd) 'eshell-ls-readonly)
                  (t 'eshell-ls-missing)))
      (concat
       (propertize short-pwd 'face pwd-face 'readonly t)
       (propertize (if (= (user-uid) 0) " # " " $ ")
                   'face 'eshell-prompt
                   'readonly t
                   'rear-nonsticky '(face readonly)))))
  (setq eshell-prompt-function 'z-eshell-prompt-function
        eshell-highlight-prompt nil
        eshell-cmpl-cycle-completions nil)

  (defalias 'eshell/x 'eshell/exit)
  (defun eshell/p (&rest args)
    "Call find-file-read-only on files"
    (mapc #'find-file-read-only
          (mapcar #'expand-file-name
                  (eshell-flatten-list (reverse args)))))

  (defun eshell/ec (&rest args)
    "Call find-file-read-only on files"
    (mapc #'find-file
          (mapcar #'expand-file-name
                  (eshell-flatten-list (reverse args)))))
  (defun z-eshell-mode-hook ()
    ;;(company-mode 1)
    ;; somehow eshell-mode-map is buffer-local
    (bind-keys :map eshell-mode-map
             ("M-r" . counsel-esh-history)))
  (add-hook 'eshell-mode-hook #'z-eshell-mode-hook))

(use-package scala2-mode
  :config
  (defun z-scala-mode-hook ()
    (setq prettify-symbols-alist
          (append '(("=>" . ?⇒)
                    ("->" . ?→))
                  prettify-symbols-alist))
    (prettify-symbols-mode))
  (add-hook 'scala-mode-hook #'z-scala-mode-hook))

(use-package calc
  :bind (("M-*" . calc-dispatch)))

(use-package calc-ext
  :config

  (defvar hydra-calc-paused nil
    "Record if hydra-calc has been paused")

  (defhydra hydra-calc (:color pink :hint nil)
    "
Notations: 3.14e6  _23=-23  3:4=¾  5:2:3=5⅔  16#12C=0x12C  [1..4)=interval
----------------  scr_o_ll: _{_↑  _<_ _>_  ↓_}_ ---------------
_U_n/_D_o        x_!_    _Q_:√    _H_yper    _S_in   ^^(2,4)=2+4i  ^^Vector    _w_hy      _y_ank-to-buf
_`_edit^^        _&_:x⁻¹ _B_:log  _I_nv      _C_os   ^^(2;4)=2e⁴ⁱ  ^^[1,2,3]   _M_:+recur
_K_eep arg^^     _%_mod  _L_n     _F_loor    _T_an   con_J_ z̄      _|_concat   _O_ption
_~_num-prefix^^  _A_bs   _E_xp    _R_ound    _f_unc  ar_G_:∠z      ^^          _p_recision  _h_elp
_=_eval-_N_um    _n_±    _P_i:π   _a_lgebra  ^^      _t_rail/time  _c_onvert   _m_ode       _i_nfo
"
    ("SPC" nil)
    ("!"  calc-factorial)
    ("%"  calc-mod)
    ("&"  calc-inv)
    ("<"  calc-scroll-left)
    ("="  calc-evaluate)
    (">"  calc-scroll-right)
    ("A"  calc-abs)
    ("B"  calc-log)
    ("C"  calc-cos)
    ("D"  calc-redo)
    ("E"  calc-exp)
    ("F"  calc-floor)
    ("G"  calc-argument)
    ("H"  calc-hyperbolic)
    ("I"  calc-inverse)
    ("J"  calc-conj)
    ("K"  calc-keep-args)
    ("L"  calc-ln)
    ("M"  calc-more-recursion-depth)
    ("N"  calc-eval-num)
    ("O"  calc-option)
    ("P"  calc-pi)
    ("Q"  calc-sqrt)
    ("R"  calc-round)
    ("S"  calc-sin)
    ("T"  calc-tan)
    ("U"  calc-undo)
    ;; ("V"               Prefix Command)
    ("X"               calc-call-last-kbd-macro)
    ;; ("Y"               Prefix Command)
    ;; ("Z"               Prefix Command)
    ("`"               calc-edit)
    ("a"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-a/body)) :exit t)
    ;; ("b"               Prefix Command)
    ("c"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-c/body)))
    ;; ("d"               Prefix Command)
    ("f"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-f/body)) :exit t)
    ;; ("g"               Prefix Command)
    ("h"               calc-help-prefix)
    ("i"               calc-info)
    ;; ("j"               Prefix Command)
    ;; ("k"               Prefix Command)
    ;; ("l"               Prefix Command)
    ("m"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-m/body)))
    ("n"               calc-change-sign)
    ("o"               calc-realign)
    ("p"               calc-precision)
    ("q"               calc-quit :exit t)
    ;; ("r"               Prefix Command)
    ("s"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-s/body)) :exit t)
    ("t"               (progn (setq hydra-calc-paused t)
                              (hydra-calc-t/body)) :exit t)
    ;; ("u"               Prefix Command)
    ;; ("v"               Prefix Command)
    ("w"               calc-why)
    ("x"               calc-execute-extended-command)
    ("y"               calc-copy-to-buffer)
    ;; ("z"               Prefix Command)
    ("{"               calc-scroll-down)
    ("|"               calc-concat)
    ("}"               calc-scroll-up)
    ("~"               calc-num-prefix))

  (defhydra hydra-calc-a (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
^^^‗‗‗‗Logical‗‗‗‗^^^   ‗‗^^Poly‗‗     ^^            ^^          ^^     ‗‗‗‗^^Numerical‗‗‗‗
_!_:¬   _._:remove=^^   ^_%_:rem       _f_actor     _s_implify/_e_xtd   _*_:∏      ^_R_oot              _M_ap eqn^^   _a_part
_&_:∧   _:_if _#_:≠     _\\_:div       _i_:∫        _c_ollect^^         _+_:∑      ^_F_it curve         mi_N_/ma_X_   su_b_stitute
_|_:∨   _{_:∈      ^^   ^_P_oly-roots  _d_/dx       _n_ormalize^^       _-_:alt∑   ^_S_olve             _m_atch^^   _A_bs
_[_:≤   _]_:≥      ^^   ^_g_cd         ̲:subᵢ^^     _r_ewrite   ^^      _I_:∫dx    _\"_:expand formula  _T_abulate^^
_<_ _=_ _>_             ^^^            _t_aylor     e_x_pand ^^         alg-e_v_aluate  _p_oly-interp
"
    ("SPC" nil)
    ("!"  calc-logical-not) ("\"" calc-expand-formula) ("#"  calc-not-equal-to)
    ("%"  calc-poly-rem) ("&"  calc-logical-and) ("*"  calc-product) ("+"  calc-summation)
    ("-"  calc-alt-summation) ("."  calc-remove-equal) ("/"  calc-poly-div-rem) (":"  calc-logical-if)
    ("<"  calc-less-than) ("="  calc-equal-to) (">"  calc-greater-than) ("?"  calc-a-prefix-help)
    ("A"  calc-abs) ("F"  calc-curve-fit) ("I"  calc-num-integral) ("M"  calc-map-equation)
    ("N"  calc-find-minimum) ("P"  calc-poly-roots) ("R"  calc-find-root) ("S"  calc-solve-for)
    ("T"  calc-tabulate) ("X"  calc-find-maximum) ("["  calc-less-equal) ("\\" calc-poly-div)
    ("]"  calc-greater-equal) ("_"  calc-subscript)
    ("a"  calc-apart)
    ("b"  calc-substitute)
    ("c"  calc-collect)
    ("d"  calc-derivative)
    ("e"  calc-simplify-extended)
    ("f"  calc-factor)
    ("g"  calc-poly-gcd)
    ("i"  calc-integral)
    ("m"  calc-match)
    ("n"  calc-normalize-rat)
    ("p"  calc-poly-interp)
    ("r"  calc-rewrite)
    ("s"  calc-simplify)
    ("t"  calc-taylor)
    ("v"  calc-alg-evaluate)
    ("x"  calc-expand)
    ("{"  calc-in-set)
    ("|"  calc-logical-or))

  (defhydra hydra-calc-t (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
‗‗‗‗^^^^Trail^^^^‗‗‗‗‗‗‗  ^^         ^^^^         ^^            ‗‗‗‗^^Time^^‗‗‗‗
_[_ _]_ first/last^^^^    _d_isplay  _m_arker^^   0-9:store^^   _-_/_+_ buzn day  _Y_ear    _T_an
_p_rev/_n_ext ^^^^        _h_ere     _k_ill^^     _._:vector    _C_onvert T_Z_    _M_onth   _U_nix
_<_ _>_ scroll ^^^^       _i_n       _y_ank^^     ^^            _N_ow^^           _W_eek    _P_art
_{_ _}_ _b_ack/_f_orward  _o_ut      _r_/_s_earch ^^            _I_nc month^^     _D_ate    _J_ulian
"
    ("SPC" nil)
    ("+"             calc-business-days-plus)
    ("-"             calc-business-days-minus)
    ("."             calc-full-trail-vectors)
    ("<"             calc-trail-scroll-left)
    (">"             calc-trail-scroll-right)
    ("?"             calc-t-prefix-help)
    ("C"             calc-convert-time-zones)
    ("D"             calc-date)
    ("I"             calc-inc-month)
    ("J"             calc-julian)
    ("M"             calc-new-month)
    ("N"             calc-now)
    ("P"             calc-date-part)
    ("T"             calc-tan)
    ("U"             calc-unix-time)
    ("W"             calc-new-week)
    ("Y"             calc-new-year)
    ("Z"             calc-time-zone)
    ("["             calc-trail-first)
    ("]"             calc-trail-last)
    ("b"             calc-trail-backward)
    ("d"             calc-trail-display)
    ("f"             calc-trail-forward)
    ("h"             calc-trail-here)
    ("i"             calc-trail-in)
    ("k"             calc-trail-kill)
    ("m"             calc-trail-marker)
    ("n"             calc-trail-next)
    ("o"             calc-trail-out)
    ("p"             calc-trail-previous)
    ("r"             calc-trail-isearch-backward)
    ("s"             calc-trail-isearch-forward)
    ("y"             calc-trail-yank)
    ("{"             calc-trail-backward)
    ("}"             calc-trail-forward))

  (defhydra hydra-calc-f (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
_A_bs²      _F_loor^^     _h_=√x²+y²   _b_eta       _s_ign^^         fp=m*10ᵉ
_e_rf       _I_nt-log^^   arc_T_an2    _g_amma      bessel-_j_/_y_   _M_antissa
_E_xp-1     int-s_Q_rt^^  _i_m         inc-_B_eta   _[_-1 _]_+1      e_X_ponent
_L_n+1      mi_n_/ma_x_   _r_e         inc-_G_amma  ^^^^             _S_cale
"
    ("SPC" nil :exit t)
    ("A"             calc-abssqr)
    ("B"             calc-inc-beta)
    ("E"             calc-expm1)
    ("F"             calc-floor)
    ("G"             calc-inc-gamma)
    ("I"             calc-ilog)
    ("L"             calc-lnp1)
    ("M"             calc-mant-part)
    ("Q"             calc-isqrt)
    ("S"             calc-scale-float)
    ("T"             calc-arctan2)
    ("X"             calc-xpon-part)
    ("["             calc-decrement)
    ("]"             calc-increment)
    ("b"             calc-beta)
    ("e"             calc-erf)
    ("g"             calc-gamma)
    ("h"             calc-hypot)
    ("i"             calc-im)
    ("j"             calc-bessel-J)
    ("n"             calc-min)
    ("r"             calc-re)
    ("s"             calc-sign)
    ("x"             calc-max)
    ("y"             calc-bessel-Y))

  (defhydra hydra-calc-m (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
‗‗Simplify^^^^‗‗    _a_lgebra    _d_eg°   auto-re_C_ompute    _g_et modes
N_O_      _A_lg     _t_otal↵     _r_adπ   settings _F_ile     save _m_odes
_N_um     _E_xt     _s_mbolic    _h_ms    _M_ore recur-dep    _X_:load all
bas_I_c   _U_nits   _p_olar      _i_nf∞   _S_hift prefix      always e_x_tensions
_B_in     _D_efault _v_:matrix   _f_rac   _w_orking           mode _R_ecord
                                                             _e_mbedded-preserve modes
"
    ("A"             calc-alg-simplify-mode)
    ("B"             calc-bin-simplify-mode)
    ("C"             calc-auto-recompute)
    ("D"             calc-default-simplify-mode)
    ("E"             calc-ext-simplify-mode)
    ("F"             calc-settings-file-name)
    ("I"             calc-basic-simplify-mode)
    ("M"             calc-more-recursion-depth)
    ("N"             calc-num-simplify-mode)
    ("O"             calc-no-simplify-mode)
    ("R"             calc-mode-record-mode)
    ("S"             calc-shift-prefix)
    ("U"             calc-units-simplify-mode)
    ("X"             calc-load-everything)
    ("a"             calc-algebraic-mode)
    ("d"             calc-degrees-mode)
    ("e"             calc-embedded-preserve-modes)
    ("f"             calc-frac-mode)
    ("g"             calc-get-modes)
    ("h"             calc-hms-mode)
    ("i"             calc-infinite-mode)
    ("m"             calc-save-modes)
    ("p"             calc-polar-mode)
    ("r"             calc-radians-mode)
    ("s"             calc-symbolic-mode)
    ("t"             calc-total-algebraic-mode)
    ("v"             calc-matrix-mode)
    ("w"             calc-working)
    ("x"             calc-always-load-extensions))


  (defhydra hydra-calc-c (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
_d_egree  _%_:percent  _p_oloar
_r_adian  _F_raction   _c_lean
_h_ms     _f_loat      _C_os
"
    ("%" calc-convert-percent)
    ("C" calc-cos)
    ("F" calc-fraction)
    ("c" calc-clean)
    ("d" calc-to-degrees)
    ("f" calc-float)
    ("h" calc-to-hms)
    ("p" calc-polar)
    ("r" calc-to-radians))

  (defhydra hydra-calc-s (:color blue :hint nil
                                 :after-exit (when hydra-calc-paused
                                               (setq hydra-calc-paused nil)
                                               (hydra-calc/body)))
    "
‗‗_s_tore‗‗   in_t_o        ‗v←_m_ap(v)‗^^^^^^  ‗‗Special Variables‗‗
0..9 quick^^  _i_nsert      _&_:1/v ^^ ^^ ^^    _A_lgSimpRules  _H_olidays    _L_ineStyles
_:_assign     _k_onstant    _+__-__*__/_        e_X_tSimpRules  _I_ntegLimit  _P_ointStyles
_=_evalto     _l_et         _[_+1 _]_-1 ^^ ^^   _E_valRules     _U_nits       Plot_R_ejects
_r_ecall      _p_ermanent   _|_concat ^^ ^^ ^^  _F_itRules      _D_ecls
_c_opy        e_x_change    _n_eg^^ ^^ ^^       _G_enCount      _T_imeZone
_d_eclare     _u_nstore     _e_dit^^ ^^ ^^
"
    ("&"             calc-store-inv)
    ("*"             calc-store-times)
    ("+"             calc-store-plus)
    ("-"             calc-store-minus)
    ("/"             calc-store-div)
    (":"             calc-assign)
    ("="             calc-evalto)
    ("A"             calc-edit-AlgSimpRules)
    ("D"             calc-edit-Decls)
    ("E"             calc-edit-EvalRules)
    ("F"             calc-edit-FitRules)
    ("G"             calc-edit-GenCount)
    ("H"             calc-edit-Holidays)
    ("I"             calc-edit-IntegLimit)
    ("L"             calc-edit-LineStyles)
    ("P"             calc-edit-PointStyles)
    ("R"             calc-edit-PlotRejects)
    ("S"             calc-sin)
    ("T"             calc-edit-TimeZone)
    ("U"             calc-edit-Units)
    ("X"             calc-edit-ExtSimpRules)
    ("["             calc-store-decr)
    ("]"             calc-store-incr)
    ("^"             calc-store-power)
    ("c"             calc-copy-variable)
    ("d"             calc-declare-variable)
    ("e"             calc-edit-variable)
    ("i"             calc-insert-variables)
    ("k"             calc-copy-special-constant)
    ("l"             calc-let)
    ("m"             calc-store-map)
    ("n"             calc-store-neg)
    ("p"             calc-permanent-variable)
    ("r"             calc-recall)
    ("s"             calc-store)
    ("t"             calc-store-into)
    ("u"             calc-unstore)
    ("x"             calc-store-exchange)
    ("|"             calc-store-concat))


  (bind-keys :map calc-mode-map
             ("a SPC" . hydra-calc-a/body)
             ("t SPC" . hydra-calc-t/body)
             ("f SPC" . hydra-calc-f/body)
             ("m SPC" . hydra-calc-m/body)
             ("c SPC" . hydra-calc-c/body)
             ("s SPC" . hydra-calc-s/body)
             ("SPC" . hydra-calc/body)
             ("M-n" . calc-trail-next)
             ("M-p" . calc-trail-previous)))

(use-package edit-server :ensure
  :diminish (edit-server-edit-mode)
  :init
  (add-hook 'after-init-hook 'edit-server-start)
  :config
  (setq edit-server-new-frame nil
        edit-server-url-major-mode-alist
        '(("mail\\.google\\.com" . html-mode)
          ("snippets\\.googleplex\\.com" . markdown-mode)
          ("b\\.corp\\.google\\.com" . gfm-mode))))

(use-package haskell-mode
  :config
  (defun z-haskell-mode-hook ()
    (haskell-indentation-mode))
  ;;(setq haskell-font-lock-symbols 't)
  (add-hook 'haskell-mode-hook #'z-haskell-mode-hook))
