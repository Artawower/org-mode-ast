import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Bold test', () => {
  it('Should parse nested text formatters', () => {
    const orgDoc = '* Hello +*world*+';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        headline [0-17]
            :level 1:
          title [0-17]
            operator [0-2] ("* ")
            text [2-8] ("Hello ")
            crossed [8-17]
              operator [8-9] ("+")
              bold [9-16]
                operator [9-10] ("*")
                text [10-15] ("world")
                operator [15-16] ("*")
              operator [16-17] ("+")
      "
    `);
  });

  it('Should parse multiple opened brackets with text as simple text', () => {
    const orgDoc = 'Hello [<+*-_world';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        text [0-17] ("Hello [<+*-_world")
      "
    `);
  });

  it('Should parse nested formatted text', () => {
    const orgDoc = '*Hello +big /world/+*';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-21]
        bold [0-21]
          operator [0-1] ("*")
          text [1-7] ("Hello ")
          crossed [7-20]
            operator [7-8] ("+")
            text [8-12] ("big ")
            italic [12-19]
              operator [12-13] ("/")
              text [13-18] ("world")
              operator [18-19] ("/")
            operator [19-20] ("+")
          operator [20-21] ("*")
      "
    `);
  });

  fit('Complex sample of real org node data', () => {
    const orgDoc = `
:PROPERTIES:
:ID: elisp
:PUBLISHED: true
:END:
#+TITLE: Elisp.
#+DESCRIPTION: (message "Язык состоящий на 30% из смайликов").
#+ID: elisp
#+AUTHOR: Darkawower
#+STARTUP: content
#+FILETAGS: :emacs:elisp:емакс:
#+ACTIVE: Yep!
#+begin_quote
Lisp — это не язык, а строительный материал. (Алан Кэй)
#+end_quote
* Elisp - расширеяемый функциональный язык для emacs
Все что я опишу ниже - плод моего изучения. Рекомендую изучать язык по ссылкам приведенным ниже. Я могу ошибаться, а также неправильно интерпритировать изученный мной материал.
Также, может показаться что я отношусь к лиспу как к неочень хорошо спроектированному языку. Это не так. Я отношусь так ко всем языкам. При этом автор понятия не имеет как можно что-то улучшить, и вообще... не стоит тратить время на его писульки.

** Ссылки
#+START_{SPOILER} Ресурсы для ознакомления >

+ [[htest-varps://bzg.fr/en/learn-emacs-lisp-in-15-minutes/][Emacs lisp за 15 минут]]
+ [[htest-varps://www.gnu.org/software/emacs/manual/][Emacs manual]]
+ [[htest-varp://www.paulgraham.com/onlisp.html][O lisp, книга про лисп, доступна в формате org mode]]
+ [[htest-varps://www.emacswiki.org/emacs/UnitTesting][Unit тесты]]
+ [[htest-varps://www.emacswiki.org/emacs/RegularExpression][Regexp]]
+ [[htest-varps://github.com/melpa/melpa/blob/master/CONTRIBUTING.org][Хорошее описание как сделать и законтрибутить свой пакет]]
+ [[htest-varps://dev.to/viglioni/how-i-set-up-my-emacs-for-typescript-3eeh][React + typescript emacs]]
+ [[htest-varps://www.reddit.com/r/emacs/comments/r6tq00/is_there_a_way_to_check_if_the_running_emacs_has/][Проверить флаги компиляции emacs]]
+ [[https://github.com/jwiegley/emacs-async][Асинхронность в elisp]]
+ [[https://github.com/doublep/eldev][Eldev - преблуда для разработки]]
+ [[https://www.reddit.com/r/emacs/comments/s0zvlh/formal_specification_and_programmatic_parser_for/][Мысли чувака с редита про org parser]]
+ [[https://habr.com/ru/company/sberbank/blog/655509/][Уникальность языка lisp (ru, habr)]]
#+CLOSE_{SPOILER}
** Пакеты в помощь для разработки
+ [[htest-varps://gitlab.com/niklaseklund/dtache][Dtache - пакет для запуска изолированных процессов]] ([[https://www.youtube.com/watch?v=if1W58SrClk][тут можно посмотреть]])
* Quick Start.
Быстрый старт для тех кто уже умеет программировать.

** Типы данных, переменные, константы
#+START_{SPOILER} Основа языка >

*** Объевление переменной
Такие переменные объявляются в глобальном скоупе (либо переопределяется в локальном. Локальный скоуп - =let=, но об этом ниже). Т.к. в лиспе нет изоляции на уровне модуля то хорошей практикой является использование префиксов. Часто префиксы сопостовимы с название пакета. Например *ivy*--sort.

#+begin_src emacs-lisp
(setq var "my-package--variable")
#+end_src
*** Defcustom
Переменные которые могут настраиваться с помощью *customize* - специального меню с ui полями ввода.
Значение для переменной =defcustom= можно выбирать из списка: =:options=. Разработчик плагина может заранее задать список возможных значений для таких перменных.
=:group= - значение которое позволяет группировать несколько переменных в группу, для более удобного редактирования.
Как я понял =defcustom= почти всегда > =setq=.

#+begin_src emacs-lisp
(defcustom my-custom-variable "hello"
  "Some description"
  :type 'string
  :group 'my-custom-group)

(message my-custom-variable)
#+end_src
#+RESULTS: 
: hello
*** Объявление локальной переменой
При любой возможности стоит выбирать локальную переменную, т.к. это изолирует функционал и сводит случайную перезапись к минимуму.

#+begin_src emacs-lisp
  (let ((my-var "I'am a local variable"))
     (message my-var))
#+end_src
#+RESULTS: 
: I’am a local variable

Существует ограничение, такую переменную нельзя периспользовать в блоке =let=. Чтобы ее можно было переиспользовать используется =let*=. Лично я используею везде =let*=.

#+begin_src emacs-lisp
(let* ((my-var "I'am a local variable")
       (my-var (concat my-var " And i can be overwrited!")))
  (message my-var))
#+end_src
#+RESULTS: 
: I’am a local variable And i can be overwrited!
Локальную переменную можно перезаписать, иногда это поволяет сократить избыточный код. =setq= в данном случае перезапишет *локальный* скоуп.

#+begin_src emacs-lisp
(let* ((name "Oleg"))
  (message name)
  (setq name "Vasya")
  (message name))
#+end_src
#+RESULTS: 
: Vasya
*** Объявление локальной переменной, выполняя операцию в случае если оно не nil
#+begin_src emacs-lisp
(when-let* ((b 4)
            (d nil))
  (message "This code never will be executed"))

(when-let* ((b 4)
            (d "He"))
  (message "But this code will be!"))
#+end_src
#+RESULTS: 
: But this code will be!
*** Работа с char
Char в лиспе обозначается знаком вопроса.
Конвертация осуществляется с помощью функции =string=, либо если это список из символов то с помощью функции =concat=

#+begin_src emacs-lisp

(let ((my-awesome-char ?Q))
              (message (string my-awesome-char ?H ?e ?e ?l ?o))
              (message (concat '(?W ?o ?r ?l ?d))))
#+end_src
#+RESULTS: 
: World
С помощью символов можно сделать repeat

#+begin_src emacs-lisp
(make-string 10 ?|)
#+end_src
#+RESULTS: 
: ||||||||||
*** Работа со строками
*Форматирование строки*

#+begin_src emacs-lisp
(message (format "Hello %s\n" "World?"))
#+end_src
#+RESULTS: 
: Hello World?
*** Списки
Списки "экранируются" (на самом деле это не экранирование, т.к. все в лиспе функция это просто указатель на то что это не нужно исполнять, называется это evaluate /но это конечно же не точно/) с помощью симола '

#+begin_src emacs-lisp
(setq my-first-list '("Foo" "Baz" "Qwe"))
#+end_src
**** Получить первый элемент
#+begin_src emacs-lisp
(car my-first-list)
#+end_src
#+RESULTS: 
: Foo
**** Получить все кроме первого элемента..
#+begin_src emacs-lisp
(cdr my-first-list)
#+end_src
**** Добавить элемент в список
Push мутирует список. Не выглядит как нечто функциональное, но возможно я что-то не понял.

#+begin_src emacs-lisp
(setq my-first-list '())
(push "Lalalend" my-first-list)
(push "Hey" my-first-list)
#+end_src
| Hey | Lalalend |
Ну или так (последний аргумент t - добавить в конец)

#+begin_src emacs-lisp
(setq my-test-2-list '("qweqweqwe" "123"))
(add-to-list 'my-test-2-list "qwe" t)

(message "%s" my-test-2-list)
#+end_src
#+RESULTS: 
: (qweqweqwe 123 qwe)
**** Слияние 2 списков
#+begin_src emacs-lisp
(setq my-first-list '(?q ?b ?c))
(setq my-first-list (append my-first-list (list ?t)))
(message "%s" my-first-list)
#+end_src
#+RESULTS: 
: (113 98 99 116)
**** Map
На самом деле =mapcar= +(возможно создатель языка хотел иметь машину...).+

#+begin_src emacs-lisp
  (defun greeting (name)
    (format "Hello %s" name))
  
  (mapcar 'greeting my-first-list)
#+end_src
**** forEach
=mpcar= создает новый список, можно просто итерироваться по записям с помощь. dolist

#+begin_src emacs-lisp
(let* ((v ""))

  (dolist (p '("one" "two" "three"))
    (setq v (concat v " " p)))
  (message v))
#+end_src
#+RESULTS: 
:  one two three
**** Проверить есть ли элемент в списке
#+begin_src emacs-lisp
(member "123" '(1233 "qwe" "123"))
#+end_src
| 123 |
**** Перезаписать элемент в списке по индексу
#+begin_src emacs-lisp
(setq my-test-list '(("qwe" . 1) ("be" . 2)))
(setcdr (assoc "qwe" my-test-list) "asdlkajsdakd")
(message "%s" my-test-list)
#+end_src
#+RESULTS: 
: ((qwe . asdlkajsdakd) (be . 2))

А что если этого элемента нет в списке?
#+begin_src emacs-lisp
(setq my-test-list '(("be" . 2)))
(setcdr (assoc "qwe" my-test-list) "asdlkajsdakd")
(message "%s" my-test-list)
#+end_src

Не работает
**** Удалить элемент из списка
#+BEGIN_SRC emacs-lisp :results silent
ELISP> (setq list1 '(alpha beta gamma))
 (alpha beta gamma)
 
 ELISP> (setq list2 (delete 'beta list1))
 (alpha gamma)
 
 ELISP> (setq list3 (delete 'alpha list1))
 (gamma)
#+END_SRC


*** Ассоциативные массивы
**** Объявление
#+begin_src emacs-lisp
(setq trees '((a . 1) (b . "qwe")))
#+end_src
#+RESULTS: 
: ((a . 1) (b . qwe))

При чем точка нужна для специального типа /symbols/. Если работает с реальными значениями то можно и без нее

#+begin_src emacs-lisp
(setq another-hashmap '(("a" "First elem") ("b" "Second elem")))
#+end_src
**** Получить элемент по ключу
#+begin_src emacs-lisp
(message "%s" (assoc 'a trees))
#+end_src
Ну и конечно возвращает оно кортеж..а чтобы получить элемент нужно использовать уже известную нам функцию - =cdr=

#+begin_src emacs-lisp
(message "%s" (cdr (assoc 'a trees)))
#+end_src
#+RESULTS: 
: 1
**** Получить элемент по значению
#+begin_src emacs-lisp
(message "%s" (rassoc "qwe" trees))
#+end_src
При этом /rassoc/ работает и для строк и для чисел, а вот /rassq/ только для чисел

#+begin_src emacs-lisp
(message "%s" (rassq "qwe" trees)) ;; nil
(message "%s" (rassq 1 trees)) ;; (a . 1)
#+end_src
#+RESULTS: 
: (a . 1)
**** Копирование мапы
#+begin_src emacs-lisp
  (setq needles-per-cluster
        '((2 . ("Austrian Pine" "Red Pine"))
          (3 . ("Pitch Pine"))
          (5 . ("White Pine"))))
  (setq copy (copy-alist needles-per-cluster))
  (message "%s" copy)
#+end_src
#+RESULTS: 
: ((2 Austrian Pine Red Pine) (3 Pitch Pine) (5 White Pine))
**** Удаление всех записей по ключу
#+begin_src emacs-lisp
  (setq alist (list '(foo 1) '(bar 2) '(foo 3) '(lose 4)))
  (setq new-alist (assq-delete-all 'foo alist)) ;; Возвращает новое значение
  (message "%s" new-alist)
  (message (concat (format "alist: %s\n" alist)
                   (format "new: %s" new-alist)))
#+end_src
#+RESULTS: 
: alist: ((foo 1) (bar 2) (lose 4))
:  new: ((bar 2) (lose 4))
**** Удаление записей по значению
#+begin_src emacs-lisp
  (setq alist2 '((foo . first) (bar . second) (foo2 . third) (qwe . five)))
  (setq new-alist (rassq-delete-all 'third alist2)) ;; меняет значение ?
  (message "%s" new-alist)
  (message (concat (format "alist: %s\n" alist2)
                   (format "new: %s" new-alist)))
  ;; (message "%s" (rassq 'foo alist2))
#+end_src
#+RESULTS: 
: alist: ((foo . first) (bar . second) (qwe . five))
: new: ((foo . first) (bar . second) (qwe . five))
*** Хешмап
[[htest-varp://ergoemacs.org/emacs/elisp_hash_table.html][Документация]]

#+begin_src emacs-lisp
  (setq my-first-map #s(
                        hash-table
                        size 10
                        test equal
                        data (
                              python-mode "spam!"
                              go-mode "booo!1 terrible pointer"
                              org-mode "amma fluffy feature ;p"
                              )))
  (puthash 'js-mode "ugly language" my-first-map)
  (message "%s" (gethash 'python-mode my-first-map))
  (message "%s" (gethash 'js-mode my-first-map))
#+end_src
#+RESULTS: 
: ugly language
*** Символ
Тип данные соотвутствующий объекту с именем. Задаются символы с помощью 1 начальной кавычки. ='amma-symbol=

#+CLOSE_{SPOILER}
** Функции
#+START_{SPOILER} Читать про функции >

*** Объявление функций
Функции принято комментировать, это позволяет смотреть документацию в автодополнении.
Вызов =(interactive)= означается что функция публичная и может быть взывана пользователем напрямую, либо через сочетание клавиш.

#+begin_src emacs-lisp
  (defun hello (my-name)
    "This function will say hello for MY-NAME."
    (interactive)
    (message (concat "Hello, I'am " my-name)))

  (hello "Artur")
#+end_src
#+RESULTS: 
: Hello, I’am Artur
*** Опицональные аргументы
#+begin_src emacs-lisp
(defun my-super-optional-function (name &optional last-name patronymic)
  (message "%s %s %s" name (or last-name "") (or patronymic "")))

(my-super-optional-function "Artur" nil "Proshkov")
#+end_src
#+RESULTS: 
: Artur  Proshkov
*** Именованные аргументы
#+begin_src emacs-lisp
(defun my-super-function-with-named-args (&rest args)
  (message "Name %s, middle name %s" (plist-get args :name) (plist-get args :middle-name)))

  (my-super-function-with-named-args :name "One" :middle-name "Dude")
#+end_src
#+RESULTS: 
: Name One, middle name Dude
*** Лямбды
+Очевидно, лямбды нужны чтобы код можно было хуже читать+

#+begin_src emacs-lisp
(funcall '(lambda () (message "I'am dirty func")))
#+end_src
#+RESULTS: 
: I’am dirty func
*** Advice
Адвайсы это прокаченные декораторы. Могут быть вызваны как до так и после вызова оригинальной функции.

#+begin_src emacs-lisp
(defun my-increment (n)
  (+ n 1))

(defun mux-5 (n)
  (* n 5))

(advice-add 'my-increment :filter-return #'mux-5)
(message "%s" (my-increment 10))
#+end_src
#+RESULTS: 
: 55
*Пример адвайса после выполненеия функции*

#+begin_src emacs-lisp
(defun my-first-func()
  (message "qweqwe"))
(my-first-func)
(defun my-adv()
  (message "advice called"))
(advice-add :after 'my-first-func #'my-adv)
(my-first-func)
#+end_src
#+RESULTS: 
: qweqwe
*** Property list (plist)
*Установка и запись*

#+begin_src emacs-lisp
(setq my-plist '(:is-enabled t :another-prop "hey"))
(message "enabled: %s, another prop: %s, type: %s" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop) (type-of my-plist))
#+end_src
#+RESULTS: 
: enabled: t, another prop: hey, type: cons

*Изменение*

#+begin_src emacs-lisp
(setq my-plist '(:is-enabled t :another-prop "hey"))

(plist-put my-plist  :another-prop "Wow, i was changed")
(message "enabled: %s, another prop: %s" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop))
#+end_src
*Итерация по plist*

#+begin_src emacs-lisp
(setq my-plist '(:is-enabled t :another-prop "hey"))

(setq res "res: ")
(loop for (k v) on my-plist by 'cddr do
      (setq res (concat res (format "%s - %s" k v) "\n")))

;; (mapcar (lambda (k) (setq res (concat res (format "%s - " k ) "\n"))) my-plist)


;; (dolist (p my-plist)
;;   (setq res (concat res (format "%s" p) "\n")))

(message res)
#+end_src
*Удаление элемента из plist*

#+begin_src emacs-lisp
(setq test '(:hi "there" :by "man!"))

(setq test (map-delete test :hi))

(message "res: %s" test)
#+end_src
#+RESULTS: 
: res: (:by man!)

#+CLOSE_{SPOILER}
*** [[htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html][Отложенный запуск функций]]
#+begin_src emacs-lisp
(setq my-custom-timer (run-with-idle-timer 1 nil #'(lambda () (message "qwe"))))
#+end_src
#+RESULTS: 
: [nil 0 1 0 nil (lambda nil 'message "qwe") nil idle 0 nil]

Отложенные функции можно отменять

#+begin_src emacs-lisp
(cancel-timer my-custom-timer)
#+end_src
** Операторы
Орпеторы это точно такие же функции. Вынес в отдельную категорию т.к. в большинстве языков это инструкции.
#+START_{SPOILER} Детали >

*** Switch case
#+begin_src emacs-lisp
(setq test-var 'qwe)
(message "%s" (cond ((eq test-var 'q2e) 1)
       ((eq test-var 'oe) 2)
       (t "qwe")))
#+end_src
#+RESULTS: 
: qwe
*** While
#+begin_src emacs-lisp
(setq my-counter 0)
(while (< my-counter 12)
         (setq my-counter (+ my-counter 1)))

(message "%s" my-counter)
#+end_src
#+RESULTS: 
: 12
*** Catch
Просто вау, в фп есть try catch! Я действительно удивлен..даже в объектно ориетированых языках это вызывает проблемы..тем не менее..это 1 из вариантов прерываия цикла while (плохихи вариатов, как по мне, но все же)

#+begin_src emacs-lisp
(setq my-counter 0)


(message "What is the messafe from catch? Oh this is message: %s" (catch 'result
  (while (< my-counter 22)
    (setq my-counter (+ my-counter 1))
    (if (> my-counter 5)
        (throw 'result "Amma result from catch block"))
    )))
#+end_src
*** Return
Работает в *emacs 27.1+*. Позволяет прервать выполнение функции.

#+begin_src emacs-lisp
(setq my-counter 0)
(cl-defun my-iterator()
  (while (< my-counter 12)
    (if (> my-counter 3)
        (return-from my-iterator)
      )
    (setq my-counter (+ my-counter 1)))
  )

(my-iterator)

(message "%s" my-counter)
#+end_src
#+RESULTS: 
: 4

#+CLOSE_{SPOILER}
** Взаимодействие с emacs
#+START_{SPOILER} Детали >

*** Вставка в текста
#+begin_src emacs-lisp
(insert "Hello" " " "World")
#+end_src
*** Работа с буфером
**** Программное создание нового буфера
#+begin_src emacs-lisp
  (switch-to-buffer-other-window "*my-first-buffer*")
  (insert "Congratulations! I'am a new buffer")
#+end_src
**** Очистка буфера
#+begin_src emacs-lisp
(erase-buffer)
#+end_src
**** Интерактивный ввод
#+begin_src emacs-lisp
  ;; (read-from-minibuffer "Enter your name: ")
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
      (switch-to-buffer-other-window "*Your personal info")
  (erase-buffer)
  (insert (format "Hello %s!" your-name))
  (other-window 1))
#+end_src
#+RESULTS:
*** Replace в буфере
#+begin_src emacs-lisp
  (defun detect-bad-boys ()
    (setq lesson-list '("Buzova" "Volodin" "Pupin"))
  
    (defun mark-as-bad (name)
      (insert (format "Bad boy %s \n" name)))
  
    (switch-to-buffer-other-window "*lisp lesson*")
    (mapcar 'mark-as-bad lesson-list)
    (goto-char (point-min))
    (while (search-forward "Bad")
      (replace-match "Awful"))
    (other-window 1)
    )
  (detect-bad-boys)
#+end_src
*goto-char* - переход к конкретному символу
*point-min* - начало буфера
*** Добавление свойств для текста
/Перед этим необходимо запустить предыдущую функцию/

#+begin_src emacs-lisp
  ;; (detect-bad-boys)
  
  
  (defun boldify-bad-boys ()
    (switch-to-buffer-other-window "*lisp lesson*")
    (goto-char (point-min))
    (while (re-search-forward "Awful boy \\(.+\\)" nil t)
      (message (format "Its %s" (match-beginning 1)))
      (add-text-properties (match-beginning 1)
                           (match-end 1)
                           (list 'face 'bold-italic)))
    ;; (other-window 1)
    )
  
  (boldify-bad-boys)
#+end_src
#+RESULTS: 
Про сумасшедшие регекспы

#+begin_quote
;; The regular expression is "Bonjour \\(.+\\)!" and it reads:
;; the string "Bonjour ", and
;; a group of           | this is the \\( ... \\) construct
;;   any character      | this is the .
;;   possibly repeated  | this is the +
;; and the "!" string.
#+end_quote
*** Создание кнопочки
Даннный метод создает кнопку над текстом с позиции от 1 до 10.

#+begin_src emacs-lisp
(defun butest-varon-pressed (button)
  (message (format "Butest-varon pressed!")))

(define-butest-varon-type 'custom-button
  'action 'butest-varon-pressed
  'follow-link t
  'help-echo "Click Butest-varon"
  'help-args "test")

(make-butest-varon 1 10 :type 'custom-button)
#+end_src
Данная функция вставляет кнопку под текущей позицей каретки.

#+begin_src emacs-lisp
(insert-butest-varon "Press me"
               'action (lambda (_arg) (print "You are press the butest-varon!")))
#+end_src
#+RESULTS: 
: #<overlay from 1 to 10 in elisp.org>
*** Чтение из completion
#+begin_src emacs-lisp
(completing-read "Choose one: " '("foo" "bar" "baz"))
#+end_src
*** Пользовательский ввод
#+begin_src emacs-lisp
(message "U say: %s" (read-string "Say me something: "))
#+end_src
#+RESULTS: 
: U say: Ну че тут скажешь
*** Работа с выделенным текстом
**** Проверка что что-то выделено
=(use-region-p)=
**** Получить выделенный текст
#+begin_src emacs-lisp
(regionp (buffer-substring start end))
#+end_src
*** Конвертация символа в строку (ну и назад)
#+begin_src emacs-lisp
(symbol-name 'something) ;; Символ в строку
(intern (symbol-name 'something)) ;; Строка в символ
#+end_src
#+RESULTS: 
: something

#+CLOSE_{SPOILER}
*** Overlay
Overlay это очень крутая тема. Он позволяет рендерить текст который не изменяет контент реального буфера. Это может быть полезно для показа подсказок, дебага, расчитанных значений.

**** Создание оверлея в конце строки
#+begin_src emacs-lisp
(setq my-first-overlay (make-overlay (line-end-position) (line-end-position)))
#+end_src
**** Курсор заходит за предел оверлея
В моем случае курсор выходил за предел оверлея. Решается весьма просто: вставляемый в оверлей текст необходимо наделить свойством ='cursor t=

#+begin_src emacs-lisp
(setq my-popup-message (propertize popup-message 'face 'blamer--face 'cursor t))
#+end_src
**** Изменение свойств overlay
#+begin_src emacs-lisp
    (overlay-put blamer--current-overlay 'after-string my-popup-message)
    (overlay-put blamer--current-overlay 'intangible t)
    (overlay-put blamer--current-overlay 'face 'bold)
    (overlay-put blamer--current-overlay 'cursor-intangible t)
#+end_src
**** Удаление существующего оверлея
#+begin_src emacs-lisp
(if my-first-overlay
        (delete-overlay my-first-overlay))
#+end_src
*** Создание своего minor-mode :WIP:
[[htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Minor-Modes.html][Документация]]
** Работа с датами
[[htest-varps://stackoverflow.com/questions/4242012/how-do-i-add-dates-in-emacs-using-emacs-lisp][Ага, любимый стаковерфлоу]]

#+begin_src emacs-lisp
(setq t3 (time-subtract (current-time) (days-to-time 2)))
#+end_src
| 24939 | 1255 | 721279 | 0 |
#+begin_src emacs-lisp

(message "%s" (/ (float-time (time-since t3)) (* 60)))
#+end_src
#+RESULTS: 
: 2940.0710639333333
** Regexp
*** Примеры
Просто кучка примеров из разработанного мной [[htest-varps://github.com/Artawower/turbo-log][пакета]]. Регекспы весьма похожи на то что представлено в других языках. Сложно лишь работать с интерполяцией строк (неочевидна работа с большим количеством слешей в исполняемом коде.)

#+begin_src emacs-lisp
(string-match "^\\([[:blank:]]\\)*\\(return\\)" "  return {
  name: 2
}")
#+end_src
#+RESULTS: 
: 0


#+begin_src emacs-lisp
(replace-regexp-in-string "[[:blank:]]*=[[:blank:]]*.+" "" "    this.myVariable = somethingElse;")
#+end_src
#+begin_src emacs-lisp
(replace-regexp-in-string "\\(const\\|let\\|public\\|protected\\|private\\|var\\)[[:blank:]]*" "" "let anotherOne = userName")
#+end_src
#+RESULTS: 
: iable = userName
*** Regexp с группировкой
#+begin_src emacs-lisp
(concat "^(?\\(?1:[^\s]+\\) [^\s]*[[:blank:]]?\(\\(?2:[^\n]+\\)"
          "\s\\(?3:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
          "\s\\(?4:[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")
#+end_src
#+begin_src emacs-lisp
(setq test-string "feature/VW-221")
(string-match "\\(?1:[A-Za-z0-9]+\/\\)\\(?2:VW-[0-9]+\\)" test-string)
(message "res " (match-string 1 test-string))
#+end_src
#+RESULTS: 
: res
** Стандартные хуки
[[htest-varps://runebook.dev/ru/docs/elisp/standard-hooks][Просто смотри сюда]]
** Custom modes
*** Minor mode
Для того чтобы сделать свой minor mode достаточно его объявить и описать логику включения/выключений режима

#+begin_src emacs-lisp
;;;###autoload
(define-minor-mode wakatime-ui-mode
  "Wakatime ui mode. Add time track to doom modeline.
TODO:
Add support for other modeline in future."
  :init-value nil
  :global t
  :lighter nil
  :group 'wakatime-ui
  (if wakatime-ui-mode
      (wakatime-ui--watch-time)
    (wakatime-ui--stop-watch-time)))
#+end_src
Где:

=init-value= - значение по умолчанию
=global= - должен ли быть вызван глобальный мод перед локальным?
=lighter= - определяет что отображать в modeline когда мод включен
** Window
*** Получение ширины текущего экрана
=(window-total-width)=
** Асинхронное исполнение. Process.
Создание асинхронного процесаа ([[htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html][подробнее тут)]]

#+begin_src emacs-lisp
(setq process (start-process
                    "WakatimeUI"
                    wakatime-ui--buffer
                    (wakatime-find-binary)
                    (plist-get wakatime-ui--command-args :today-time)))
#+end_src
Чтение выходных данных из процесса

#+begin_src emacs-lisp

#+end_src
** Keymaps
*** Создание своего keymap
#+begin_src elisp
(with-current-buffer "*Messages*"
  (read-only-mode -1)
  (erase-buffer))

(setq my-mode-map (make-sparse-keymap))
(define-key my-mode-map (kbd "C-c C-'") 'my-mode-cmd1)
(define-key my-mode-map (kbd "C-c C-b") 'my-mode-cmd2)
(define-key my-mode-map (kbd "C-c C-c") 'my-mode-cmd3)
(define-key my-mode-map (kbd "<mouse-1>") 'my-mode-cmd4)
;; by convention, major mode's keys should begin with the form C-c C-‹key›

;; (dolist (m my-mode-map)
;;   (message "key: %s" m))





(map-keymap '(lambda (v g)
               (message "%s: %s" v g)) my-mode-map)
#+end_src
#+RESULTS:
** Macro
:PROPERTIES:
:ID: elisp-macros
:END:
Подробнее [[htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Macros.html][тут]].

*** Простой макрос
#+begin_src emacs-lisp
(defmacro inc (var)
  (list 'setq var (list '1+ var)))

(setq test-var 10)
(message "%s" (inc test-var))
#+end_src
#+RESULTS: 
: 11
*** Изучить макрос
Macroexpand - показывает вывод макроса

#+begin_src emacs-lisp
(message "%s" (macroexpand '(inc test-var)))
#+end_src
#+RESULTS: 
: (setq test-var (1+ tt))
*** Цепочка из макросов
Macroexpand отображает только первый макрос, чтобы вызвать всю цепочку используем =macroexpand-all=

#+begin_src emacs-lisp
(defmacro inc2 (var1 var2)
    (list 'progn (list 'inc var1) (list 'inc var2)))


(message "%s" (macroexpand-all '(inc2 r s)))
#+end_src
#+RESULTS: 
: (progn (setq r (1+ r)) (setq s (1+ s)))
*** Пример с более сложными конструкциями
#+begin_src emacs-lisp
(defmacro t-becomes-nil (var)
  \`(if (eq ,var t)
       (setq ,var nil)))

(message "%s" (t-becomes-nil test-var))
#+end_src
#+RESULTS: 
: nil
*** Динамическое получение переменной
 [[https://stackoverflow.com/questions/24433035/combining-two-variables-into-one-function-name-in-macro][Подсмотрено тут]]
 Чертовая магия 😮

#+begin_src emacs-lisp
(setq my-custom-variable "Hello, amma variable")

(defmacro get-with-prefix (var-name)
  \`(symbol-value (intern (concatenate 'string "my-custom" "-" (symbol-name ',var-name)))))

(get-with-prefix variable)
#+end_src
#+RESULTS: 
: Hello, amma variable

А теперь из plist, если нет - то из глобального скоупа, это еще большая магия. Да, наверное такое не стоит использовать в реальных проектах, но как же руки чешутся 😍

#+begin_src emacs-lisp
(setq my-custom-variable "Hello, amma variable")

(setq my-plist-with-prop '(:custom-variable nil :test t))

(defmacro get-with-prefix (my-plist var-name)
  \`(or (plist-get ,my-plist (symbol-value (intern (concatenate 'string ":" (symbol-name ',var-name)))))
       (symbol-value (intern (concatenate 'string "my" "-" (symbol-name ',var-name))))))

(message "%s" (get-with-prefix my-plist-with-prop custom-variable))
#+end_src
#+RESULTS: 
: Hello, amma variable
*** Передача тела (@body) :noexport:
Пожалуй самая впечатлаяющая фича (имхо, без нее смысл в макросах бы отпал). Макрос склеивает результаты выполнения функций (подумал для org-mode самое то)

#+begin_src emacs-lisp
(setq test-var 0)
(defmacro for (var from init to final do &rest body)
  \`(let ((,var ,init))
     (while (<= ,var ,final)
       ,@body
       (setq ,var (1+ ,var)))))


(for j from 0 to 4 do
     (setq test-var (+ test-var j))
     (setq test-var (/ test-var 2)))

(message "HAVA: %s" test-var)
#+end_src
#+RESULTS: 
: HAVA: 3


**** Failed :noexport:
Пример макроса, чтобы наглядно видеть в орге какая функция что делает

#+begin_src emacs-lisp
(defmacro pretty-log (&rest body)

  (let ((res (concat (make-string 80 ?-) "\n")))
    (dolist (f body)
      (setq res (concat res (format "[%s]: %s\n" f (eval f)))))
    (message res)))

(pretty-log (+ 1 12)
            (- 44 22)
            (+ (/ 12 2) (* 33 4))
            (setq ttt 12))
#+end_src
#+RESULTS: 
: --------------------------------------------------------------------------------
: [(+ 1 12)]: 13
: [(- 44 22)]: 22
: [(+ (/ 12 2) (* 33 4))]: 138
: [(setq ttt 12)]: 12
*** Модификация plist через список динамических аргументов как в use-package :noexport:
#+begin_src emacs-lisp
(setq res "")
(setq test-alist
      '((js-mode (:loggers '("hi there") :msg-format-template "Hi" :argument-divider "|"))
        (typescript-mode (:loggers '("another on", "and me") :msg-format-template "bee"))
        ))

(defmacro turbo-log-configure (&rest configs)
  (let* ((strategy (or (plist-get configs :strategy) 'replace))
         (excluded-keys '(:modes :strategy))
         (modes (plist-get configs :modes))
         current-config)

    (dolist (k excluded-keys)
      (setq configs (map-delete configs k)))

    (dolist (mode modes)
      (unless (assoc mode test-alist)
        (push \`(,mode nil) test-alist))

      (setq current-config (car (cdr-safe (assoc mode test-alist))))

      (if (eq strategy 'replace)
          (setq current-config configs)

        (loop for (k v) on configs by 'cddr do
              (if current-config
                  (plist-put current-config k v)
                (setq current-config \`(,k ,v)))))

      (message "QQQ: %s" configs)
      (if (assq mode test-alist)
          (setcdr (assq mode test-alist)
                  \`(,current-config))
        \`(push '(,mode '(,current-config)) ,test-alist))
      )))

(turbo-log-configure
 :modes (typescript-mode js2-mode js-mode)
 ;; :modes (typescript-mode j-mode)
 ;; :modes (js-mode)
 :strategy replace

 :loggers ("console.print" "console.dbg")
 :msg-format-template "\"HELLO WORLD: %s\"")

(message "-------------------------------------------------------")
(message "%s" (pp test-alist))
#+end_src
#+RESULTS: 
: ((mode nil)
:  (js-mode
:   (:loggers
:    '("hi there")
:    :msg-format-template "Hi"))
:  (typescript-mode
:   (:loggers
:    ("console.print" "console.dbg")
:    :msg-format-template "\"HELLO WORLD: %s\"")))
* Создание своего пакета
** Проверка ошибок компиляции
#+begin_src bash
emacs -Q --batch \
    --eval '(setq byte-compile-error-on-warn t)' \
    -f batch-byte-compile turbo-log.el
#+end_src
** Contribute
[[htest-varps://github.com/leotaku/elisp-check]]
** CI
[[htest-varps://github.com/a13/reverse-im.el/blob/master/.github/workflows/check.yml][Пример github actions]]
[[htest-varps://github.com/leotaku/elisp-check][Про elisp check]]
* Тесты
Тесты пишутся весьма просто. От части потому что не нужно мокать кучу зависимостей. Функция в большинстве случаев самодостаточна.

#+begin_src emacs-lisp
(ert-deftest my-first-test ()
  (should (= (+ 10 10) 20)))
#+end_src
Запуск.

#+begin_src bash
emacs -batch -l ert -l package.el -l test.el -f ert-run-tests-batch-and-exit
#+end_src
#+BEGIN_{HIDDEN}
* Статический анализ типов
[[https://github.com/emacs-elsa/Elsa][Его нет. Зато есть аннотации]]
* Временно :noexport:
#+begin_src emacs-lisp
(message "\"\\[line [0-9]+\\]\"")
#+end_src
#+begin_src emacs-lisp
(message "%s" (string-match "{\\|);?$" "public replaceNonPrintableCharacters(text: string): string {"))
#+end_src
#+RESULTS: 
: 59


#+begin_src emacs-lisp
(setq turbo-log--ecmascript-final-symbols '(?; ?)))
(while (or (not (eobp)) (member ?) '(?; ?))))
                 (setq current-char char-after))))
#+end_src
#+begin_src emacs-lisp
(setq quicktype-mode-configs '(("go" go-mode "")
                               ("ts" typescript-mode "")
                               ("js" js2-mode "")
                               ("rs" rust-mode "")
                               ("c++" c++-mode "")
                               ("javascript-prop-types" js2-mode "")
                               ("flow" flow-js2-mode "")
                               ("swift" swift-mode "")
                               ("kotlin" kotlin-mode "")
                               ("elm" elm-mode "")
                               ("ruby" ruby-mode "")
                               ("dart" dart-mode "")
                               ("py" python-mode "--python-version 3.7")
                               ("haskell" haskell-mode "")))

;; (message "%s" quicktype-mode-configs)
(message "%s" (cl-rassoc 'go-mode quicktype-mode-configs :test #'member))
;; (message "%s" (cl-rassoc "Red Pine" needles-per-cluster :test #'member))
#+end_src
#+RESULTS: 
: (go go-mode )


#+begin_src emacs-lisp
(setq needles-per-cluster
      '((2 "Austrian Pine" "Red Pine")
        (3 "Pitch Pine")
        (5 "White Pine")))

(message "%s" (cl-rassoc "Red Pine" needles-per-cluster :test #'member))
#+end_src
#+RESULTS: 
: (2 Austrian Pine Red Pine)


#+begin_src emacs-lisp
(message "%s" (string-match "\\({\\|;$\\)\\|\\(const [\\w\\[:digit]]+ = [\\d[:digit:]]+$\\)" "  const foo = 1"))
#+end_src
#+RESULTS: 
: nil
#+END_{HIDDEN}

#+begin_src emacs-lisp
(setq v (dolist (i '(1 2 3 4))
                i))
(message "%s" v)
#+end_src
#+RESULTS: 
: nil


** Check json
#+begin_src emacs-lisp
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file web-roam-configuration-file-path))
         (name-to-config (make-hash-table :test 'equal))
         (server-names '()))
    (dolist (config json)
      (message "%s" config))
  )
#+end_src
#+RESULTS:`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-32210]
        newLine [0-1]
        property [1-13]
          text [1-13] (":PROPERTIES:")
        newLine [13-14]
        property [14-24]
          text [14-18] (":ID:")
          text [18-24] (" elisp")
        newLine [24-25]
        property [25-41]
          text [25-36] (":PUBLISHED:")
          text [36-41] (" true")
        newLine [41-42]
        property [42-47]
          text [42-47] (":END:")
        newLine [47-48]
        keyword [48-63]
          text [48-56] ("#+TITLE:")
          text [56-63] (" Elisp.")
        newLine [63-64]
        keyword [64-126]
          text [64-78] ("#+DESCRIPTION:")
          text [78-126] (" (message \\"Язык состоящий на 30% из смайликов\\").")
        newLine [126-127]
        keyword [127-138]
          text [127-132] ("#+ID:")
          text [132-138] (" elisp")
        newLine [138-139]
        keyword [139-159]
          text [139-148] ("#+AUTHOR:")
          text [148-159] (" Darkawower")
        newLine [159-160]
        keyword [160-178]
          text [160-170] ("#+STARTUP:")
          text [170-178] (" content")
        newLine [178-179]
        keyword [179-191]
          text [179-190] ("#+FILETAGS:")
          text [190-191] (" ")
        operator [191-192] (":")
        text [192-197] ("emacs")
        operator [197-198] (":")
        text [198-203] ("elisp")
        operator [203-204] (":")
        text [204-209] ("емакс")
        operator [209-210] (":")
        newLine [210-211]
        keyword [211-225]
          text [211-220] ("#+ACTIVE:")
          text [220-225] (" Yep!")
        newLine [225-226]
        quoteBlock [226-307]
          blockHeader [226-239]
            keyword [226-239]
              text [226-239] ("#+begin_quote")
          newLine [239-240]
          blockBody [240-295]
            text [240-295] ("Lisp — это не язык, а строительный материал. (Алан Кэй)")
          newLine [295-296]
          blockFooter [296-307]
            keyword [296-307]
              text [296-307] ("#+end_quote")
        newLine [307-308]
        headline [308-28909]
            :level 1:
          title [308-361]
            operator [308-310] ("* ")
            text [310-360] ("Elisp - расширеяемый функциональный язык для emacs")
            newLine [360-361]
          section [361-28909]
            text [361-537] ("Все что я опишу ниже - плод моего изучения. Рекомендую изучать язык по ссылкам приведенным ниже. Я могу ошибаться, а также неправильно интерпритировать изученный мной материал.")
            newLine [537-538]
            text [538-784] ("Также, может показаться что я отношусь к лиспу как к неочень хорошо спроектированному языку. Это не так. Я отношусь так ко всем языкам. При этом автор понятия не имеет как можно что-то улучшить, и вообще... не стоит тратить время на его писульки.")
            newLine [784-785]
            newLine [785-786]
            headline [786-2285]
                :level 2:
              title [786-2285]
                operator [786-789] ("** ")
                text [789-795] ("Ссылки")
                newLine [795-796]
                keyword [796-839]
                  text [796-839] ("#+START_{SPOILER} Ресурсы для ознакомления ")
                text [839-840] (">")
                newLine [840-841]
                newLine [841-842]
                list [842-2164]
                    :unordered:
                    :level 0:
                  listItem [842-928]
                    title [842-928]
                      operator [842-844] ("+ ")
                      link [844-927]
                        operator [844-845] ("[")
                        linkUrl [845-902]
                          operator [845-846] ("[")
                          text [846-901] ("htest-varps://bzg.fr/en/learn-emacs-lisp-in-15-minutes/")
                          operator [901-902] ("]")
                        linkName [902-926]
                          operator [902-903] ("[")
                          text [903-925] ("Emacs lisp за 15 минут")
                          operator [925-926] ("]")
                        operator [926-927] ("]")
                      newLine [927-928]
                  listItem [928-997]
                    title [928-997]
                      operator [928-930] ("+ ")
                      link [930-996]
                        operator [930-931] ("[")
                        linkUrl [931-981]
                          operator [931-932] ("[")
                          text [932-980] ("htest-varps://www.gnu.org/software/emacs/manual/")
                          operator [980-981] ("]")
                        linkName [981-995]
                          operator [981-982] ("[")
                          text [982-994] ("Emacs manual")
                          operator [994-995] ("]")
                        operator [995-996] ("]")
                      newLine [996-997]
                  listItem [997-1100]
                    title [997-1100]
                      operator [997-999] ("+ ")
                      link [999-1099]
                        operator [999-1000] ("[")
                        linkUrl [1000-1045]
                          operator [1000-1001] ("[")
                          text [1001-1044] ("htest-varp://www.paulgraham.com/onlisp.html")
                          operator [1044-1045] ("]")
                        linkName [1045-1098]
                          operator [1045-1046] ("[")
                          text [1046-1097] ("O lisp, книга про лисп, доступна в формате org mode")
                          operator [1097-1098] ("]")
                        operator [1098-1099] ("]")
                      newLine [1099-1100]
                  listItem [1100-1168]
                    title [1100-1168]
                      operator [1100-1102] ("+ ")
                      link [1102-1167]
                        operator [1102-1103] ("[")
                        linkUrl [1103-1154]
                          operator [1103-1104] ("[")
                          text [1104-1153] ("htest-varps://www.emacswiki.org/emacs/UnitTesting")
                          operator [1153-1154] ("]")
                        linkName [1154-1166]
                          operator [1154-1155] ("[")
                          text [1155-1165] ("Unit тесты")
                          operator [1165-1166] ("]")
                        operator [1166-1167] ("]")
                      newLine [1167-1168]
                  listItem [1168-1238]
                    title [1168-1238]
                      operator [1168-1170] ("+ ")
                      link [1170-1237]
                        operator [1170-1171] ("[")
                        linkUrl [1171-1228]
                          operator [1171-1172] ("[")
                          text [1172-1227] ("htest-varps://www.emacswiki.org/emacs/RegularExpression")
                          operator [1227-1228] ("]")
                        linkName [1228-1236]
                          operator [1228-1229] ("[")
                          text [1229-1235] ("Regexp")
                          operator [1235-1236] ("]")
                        operator [1236-1237] ("]")
                      newLine [1237-1238]
                  listItem [1238-1368]
                    title [1238-1368]
                      operator [1238-1240] ("+ ")
                      link [1240-1367]
                        operator [1240-1241] ("[")
                        linkUrl [1241-1308]
                          operator [1241-1242] ("[")
                          text [1242-1307] ("htest-varps://github.com/melpa/melpa/blob/master/CONTRIBUTING.org")
                          operator [1307-1308] ("]")
                        linkName [1308-1366]
                          operator [1308-1309] ("[")
                          text [1309-1365] ("Хорошее описание как сделать и законтрибутить свой пакет")
                          operator [1365-1366] ("]")
                        operator [1366-1367] ("]")
                      newLine [1367-1368]
                  listItem [1368-1472]
                    title [1368-1472]
                      operator [1368-1370] ("+ ")
                      link [1370-1471]
                        operator [1370-1371] ("[")
                        linkUrl [1371-1444]
                          operator [1371-1372] ("[")
                          text [1372-1443] ("htest-varps://dev.to/viglioni/how-i-set-up-my-emacs-for-typescript-3eeh")
                          operator [1443-1444] ("]")
                        linkName [1444-1470]
                          operator [1444-1445] ("[")
                          text [1445-1469] ("React + typescript emacs")
                          operator [1469-1470] ("]")
                        operator [1470-1471] ("]")
                      newLine [1471-1472]
                  listItem [1472-1615]
                    title [1472-1615]
                      operator [1472-1474] ("+ ")
                      link [1474-1614]
                        operator [1474-1475] ("[")
                        linkUrl [1475-1579]
                          operator [1475-1476] ("[")
                          text [1476-1578] ("htest-varps://www.reddit.com/r/emacs/comments/r6tq00/is_there_a_way_to_check_if_the_running_emacs_has/")
                          operator [1578-1579] ("]")
                        linkName [1579-1613]
                          operator [1579-1580] ("[")
                          text [1580-1612] ("Проверить флаги компиляции emacs")
                          operator [1612-1613] ("]")
                        operator [1613-1614] ("]")
                      newLine [1614-1615]
                  listItem [1615-1684]
                    title [1615-1684]
                      operator [1615-1617] ("+ ")
                      link [1617-1683]
                        operator [1617-1618] ("[")
                        linkUrl [1618-1659]
                          operator [1618-1619] ("[")
                          text [1619-1658] ("https://github.com/jwiegley/emacs-async")
                          operator [1658-1659] ("]")
                        linkName [1659-1682]
                          operator [1659-1660] ("[")
                          text [1660-1681] ("Асинхронность в elisp")
                          operator [1681-1682] ("]")
                        operator [1682-1683] ("]")
                      newLine [1683-1684]
                  listItem [1684-1756]
                    title [1684-1756]
                      operator [1684-1686] ("+ ")
                      link [1686-1755]
                        operator [1686-1687] ("[")
                        linkUrl [1687-1721]
                          operator [1687-1688] ("[")
                          text [1688-1720] ("https://github.com/doublep/eldev")
                          operator [1720-1721] ("]")
                        linkName [1721-1754]
                          operator [1721-1722] ("[")
                          text [1722-1753] ("Eldev - преблуда для разработки")
                          operator [1753-1754] ("]")
                        operator [1754-1755] ("]")
                      newLine [1755-1756]
                  listItem [1756-1897]
                    title [1756-1897]
                      operator [1756-1758] ("+ ")
                      link [1758-1896]
                        operator [1758-1759] ("[")
                        linkUrl [1759-1857]
                          operator [1759-1760] ("[")
                          text [1760-1856] ("https://www.reddit.com/r/emacs/comments/s0zvlh/formal_specification_and_programmatic_parser_for/")
                          operator [1856-1857] ("]")
                        linkName [1857-1895]
                          operator [1857-1858] ("[")
                          text [1858-1894] ("Мысли чувака с редита про org parser")
                          operator [1894-1895] ("]")
                        operator [1895-1896] ("]")
                      newLine [1896-1897]
                  listItem [1897-1989]
                    title [1897-1989]
                      operator [1897-1899] ("+ ")
                      link [1899-1988]
                        operator [1899-1900] ("[")
                        linkUrl [1900-1951]
                          operator [1900-1901] ("[")
                          text [1901-1950] ("https://habr.com/ru/company/sberbank/blog/655509/")
                          operator [1950-1951] ("]")
                        linkName [1951-1987]
                          operator [1951-1952] ("[")
                          text [1952-1986] ("Уникальность языка lisp (ru, habr)")
                          operator [1986-1987] ("]")
                        operator [1987-1988] ("]")
                      newLine [1988-1989]
                  listItem [1989-2164]
                    title [1989-2164]
                      operator [1989-1991] ("+ ")
                      link [1991-2091]
                        operator [1991-1992] ("[")
                        linkUrl [1992-2038]
                          operator [1992-1993] ("[")
                          text [1993-2037] ("htest-varps://gitlab.com/niklaseklund/dtache")
                          operator [2037-2038] ("]")
                        linkName [2038-2090]
                          operator [2038-2039] ("[")
                          text [2039-2089] ("Dtache - пакет для запуска изолированных процессов")
                          operator [2089-2090] ("]")
                        operator [2090-2091] ("]")
                      text [2091-2093] (" (")
                      link [2093-2162]
                        operator [2093-2094] ("[")
                        linkUrl [2094-2139]
                          operator [2094-2095] ("[")
                          text [2095-2138] ("https://www.youtube.com/watch?v=if1W58SrClk")
                          operator [2138-2139] ("]")
                        linkName [2139-2161]
                          operator [2139-2140] ("[")
                          text [2140-2160] ("тут можно посмотреть")
                          operator [2160-2161] ("]")
                        operator [2161-2162] ("]")
                      text [2162-2163] (")")
                      newLine [2163-2164]
                keyword [2164-2181]
                  text [2164-2181] ("#+CLOSE_{SPOILER}")
                newLine [2181-2182]
                headline [2182-2216]
                    :level 2:
                  title [2007-2041]
                    operator [2007-2010] ("** ")
                    text [2010-2040] ("Пакеты в помощь для разработки")
                    newLine [2040-2041]
                  section [2041-2041]
                headline [2216-2231]
                    :level 1:
                  title [2216-2231]
                    operator [2216-2218] ("* ")
                    text [2218-2230] ("Quick Start.")
                    newLine [2230-2231]
                  section [2231-2231]
                text [2231-2283] ("Быстрый старт для тех кто уже умеет программировать.")
                newLine [2283-2284]
                newLine [2284-2285]
              section [796-796]
            headline [2285-10793]
                :level 2:
              title [2285-2357]
                operator [2285-2288] ("** ")
                text [2288-2322] ("Типы данных, переменные, константы")
                newLine [2322-2323]
                keyword [2323-2354]
                  text [2323-2354] ("#+START_{SPOILER} Основа языка ")
                text [2354-2355] (">")
                newLine [2355-2356]
                newLine [2356-2357]
              section [2323-10759]
                headline [2323-2711]
                    :level 3:
                  title [2323-2711]
                    operator [2323-2327] ("*** ")
                    text [2327-2348] ("Объевление переменной")
                    newLine [2348-2349]
                    text [2349-2452] ("Такие переменные объявляются в глобальном скоупе (либо переопределяется в локальном. Локальный скоуп - ")
                    verbatim [2452-2457]
                      operator [2452-2453] ("=")
                      text [2453-2456] ("let")
                      operator [2456-2457] ("=")
                    text [2457-2630] (", но об этом ниже). Т.к. в лиспе нет изоляции на уровне модуля то хорошей практикой является использование префиксов. Часто префиксы сопостовимы с название пакета. Например ")
                    bold [2630-2635]
                      operator [2630-2631] ("*")
                      text [2631-2634] ("ivy")
                      operator [2634-2635] ("*")
                    text [2635-2642] ("--sort.")
                    newLine [2642-2643]
                    newLine [2643-2644]
                    srcBlock [2644-2710]
                      blockHeader [2644-2666]
                        keyword [2644-2666]
                          text [2644-2655] ("#+begin_src")
                          text [2655-2666] (" emacs-lisp")
                      newLine [2666-2667]
                      blockBody [2667-2700]
                        text [2667-2700] ("(setq var \\"my-package--variable\\")")
                      newLine [2700-2701]
                      blockFooter [2701-2710]
                        keyword [2701-2710]
                          text [2701-2710] ("#+end_src")
                    newLine [2710-2711]
                  section [2349-2349]
                headline [2711-3337]
                    :level 3:
                  title [2711-3243]
                    operator [2711-2715] ("*** ")
                    text [2715-2724] ("Defcustom")
                    newLine [2724-2725]
                    text [2725-2774] ("Переменные которые могут настраиваться с помощью ")
                    bold [2774-2785]
                      operator [2774-2775] ("*")
                      text [2775-2784] ("customize")
                      operator [2784-2785] ("*")
                    text [2785-2786] (" ")
                    list [2786-2825]
                        :unordered:
                        :level 0:
                      listItem [2786-2825]
                        title [2786-2825]
                          operator [2786-2788] ("- ")
                          text [2788-2824] ("специального меню с ui полями ввода.")
                          newLine [2824-2825]
                    text [2825-2849] ("Значение для переменной ")
                    verbatim [2849-2860]
                      operator [2849-2850] ("=")
                      text [2850-2859] ("defcustom")
                      operator [2859-2860] ("=")
                    text [2860-2986] (" можно выбирать из списка: =:options=. Разработчик плагина может заранее задать список возможных значений для таких перменных.")
                    newLine [2986-2987]
                    text [2987-2996] ("=:group= ")
                    list [2996-3104]
                        :unordered:
                        :level 0:
                      listItem [2996-3104]
                        title [2996-3104]
                          operator [2996-2998] ("- ")
                          text [2998-3103] ("значение которое позволяет группировать несколько переменных в группу, для более удобного редактирования.")
                          newLine [3103-3104]
                    text [3104-3116] ("Как я понял ")
                    verbatim [3116-3127]
                      operator [3116-3117] ("=")
                      text [3117-3126] ("defcustom")
                      operator [3126-3127] ("=")
                    text [3127-3143] (" почти всегда > ")
                    verbatim [3143-3149]
                      operator [3143-3144] ("=")
                      text [3144-3148] ("setq")
                      operator [3148-3149] ("=")
                    text [3149-3150] (".")
                    newLine [3150-3151]
                    newLine [3151-3152]
                    srcBlock [3152-3222]
                      blockHeader [3152-3174]
                        keyword [3152-3174]
                          text [3152-3163] ("#+begin_src")
                          text [3163-3174] (" emacs-lisp")
                      newLine [3174-3175]
                      blockBody [3175-3212]
                        text [3175-3212] ("(defcustom my-custom-variable \\"hello\\"")
                      newLine [3212-3213]
                      blockFooter [3213-3222]
                        keyword [3213-3222]
                          text [3213-3222] ("#+end_src")
                    newLine [3222-3223]
                    keyword [3223-3234]
                      text [3223-3233] ("#+RESULTS:")
                      text [3233-3234] (" ")
                    newLine [3234-3235]
                    fixedWidth [3235-3242]
                      operator [3235-3237] (": ")
                      text [3237-3242] ("hello")
                    newLine [3242-3243]
                  section [2725-2819]
                    indent [2725-2727] ("  ")
                    text [2727-2745] ("\\"Some description\\"")
                    newLine [2745-2746]
                    indent [2746-2748] ("  ")
                    blockProperty [2748-2761]
                      text [2748-2753] (":type")
                      text [2753-2761] (" 'string")
                    newLine [2761-2762]
                    indent [2762-2764] ("  ")
                    blockProperty [2764-2788]
                      text [2764-2770] (":group")
                      text [2770-2788] (" 'my-custom-group)")
                    newLine [2788-2789]
                    newLine [2789-2790]
                    text [2790-2818] ("(message my-custom-variable)")
                    newLine [2818-2819]
                headline [3337-4299]
                    :level 3:
                  title [3337-4096]
                    operator [3337-3341] ("*** ")
                    text [3341-3371] ("Объявление локальной переменой")
                    newLine [3371-3372]
                    text [3372-3502] ("При любой возможности стоит выбирать локальную переменную, т.к. это изолирует функционал и сводит случайную перезапись к минимуму.")
                    newLine [3502-3503]
                    newLine [3503-3504]
                    srcBlock [3504-3536]
                      blockHeader [3504-3526]
                        keyword [3504-3526]
                          text [3504-3515] ("#+begin_src")
                          text [3515-3526] (" emacs-lisp")
                      newLine [3526-3527]
                      blockFooter [3527-3536]
                        keyword [3527-3536]
                          text [3527-3536] ("#+end_src")
                    newLine [3536-3537]
                    keyword [3537-3548]
                      text [3537-3547] ("#+RESULTS:")
                      text [3547-3548] (" ")
                    newLine [3548-3549]
                    fixedWidth [3549-3572]
                      operator [3549-3551] (": ")
                      text [3551-3572] ("I’am a local variable")
                    newLine [3572-3573]
                    newLine [3573-3574]
                    text [3574-3646] ("Существует ограничение, такую переменную нельзя периспользовать в блоке ")
                    verbatim [3646-3651]
                      operator [3646-3647] ("=")
                      text [3647-3650] ("let")
                      operator [3650-3651] ("=")
                    text [3651-3703] (". Чтобы ее можно было переиспользовать используется ")
                    verbatim [3703-3709]
                      operator [3703-3704] ("=")
                      text [3704-3708] ("let*")
                      operator [3708-3709] ("=")
                    text [3709-3736] (". Лично я используею везде ")
                    verbatim [3736-3742]
                      operator [3736-3737] ("=")
                      text [3737-3741] ("let*")
                      operator [3741-3742] ("=")
                    text [3742-3743] (".")
                    newLine [3743-3744]
                    newLine [3744-3745]
                    srcBlock [3745-3817]
                      blockHeader [3745-3767]
                        keyword [3745-3767]
                          text [3745-3756] ("#+begin_src")
                          text [3756-3767] (" emacs-lisp")
                      newLine [3767-3768]
                      blockBody [3768-3807]
                        text [3768-3807] ("(let* ((my-var \\"I'am a local variable\\")")
                      newLine [3807-3808]
                      blockFooter [3808-3817]
                        keyword [3808-3817]
                          text [3808-3817] ("#+end_src")
                    newLine [3817-3818]
                    keyword [3818-3829]
                      text [3818-3828] ("#+RESULTS:")
                      text [3828-3829] (" ")
                    newLine [3829-3830]
                    fixedWidth [3830-3878]
                      operator [3830-3832] (": ")
                      text [3832-3878] ("I’am a local variable And i can be overwrited!")
                    newLine [3878-3879]
                    text [3879-3966] ("Локальную переменную можно перезаписать, иногда это поволяет сократить избыточный код. ")
                    verbatim [3966-3972]
                      operator [3966-3967] ("=")
                      text [3967-3971] ("setq")
                      operator [3971-3972] ("=")
                    text [3972-4001] (" в данном случае перезапишет ")
                    bold [4001-4012]
                      operator [4001-4002] ("*")
                      text [4002-4011] ("локальный")
                      operator [4011-4012] ("*")
                    text [4012-4019] (" скоуп.")
                    newLine [4019-4020]
                    newLine [4020-4021]
                    srcBlock [4021-4075]
                      blockHeader [4021-4043]
                        keyword [4021-4043]
                          text [4021-4032] ("#+begin_src")
                          text [4032-4043] (" emacs-lisp")
                      newLine [4043-4044]
                      blockBody [4044-4065]
                        text [4044-4065] ("(let* ((name \\"Oleg\\"))")
                      newLine [4065-4066]
                      blockFooter [4066-4075]
                        keyword [4066-4075]
                          text [4066-4075] ("#+end_src")
                    newLine [4075-4076]
                    keyword [4076-4087]
                      text [4076-4086] ("#+RESULTS:")
                      text [4086-4087] (" ")
                    newLine [4087-4088]
                    fixedWidth [4088-4095]
                      operator [4088-4090] (": ")
                      text [4090-4095] ("Vasya")
                    newLine [4095-4096]
                  section [3372-3575]
                    indent [3372-3374] ("  ")
                    text [3374-3413] ("(let ((my-var \\"I'am a local variable\\"))")
                    newLine [3413-3414]
                    indent [3414-3419] ("     ")
                    text [3419-3436] ("(message my-var))")
                    newLine [3436-3437]
                    indent [3437-3444] ("       ")
                    text [3444-3497] ("(my-var (concat my-var \\" And i can be overwrited!\\")))")
                    newLine [3497-3498]
                    indent [3498-3500] ("  ")
                    text [3500-3517] ("(message my-var))")
                    newLine [3517-3518]
                    indent [3518-3520] ("  ")
                    text [3520-3534] ("(message name)")
                    newLine [3534-3535]
                    indent [3535-3537] ("  ")
                    text [3537-3556] ("(setq name \\"Vasya\\")")
                    newLine [3556-3557]
                    indent [3557-3559] ("  ")
                    text [3559-3574] ("(message name))")
                    newLine [3574-3575]
                headline [4299-4615]
                    :level 3:
                  title [4299-4467]
                    operator [4299-4303] ("*** ")
                    text [4303-4378] ("Объявление локальной переменной, выполняя операцию в случае если оно не nil")
                    newLine [4378-4379]
                    srcBlock [4379-4429]
                      blockHeader [4379-4401]
                        keyword [4379-4401]
                          text [4379-4390] ("#+begin_src")
                          text [4390-4401] (" emacs-lisp")
                      newLine [4401-4402]
                      blockBody [4402-4419]
                        text [4402-4419] ("(when-let* ((b 4)")
                      newLine [4419-4420]
                      blockFooter [4420-4429]
                        keyword [4420-4429]
                          text [4420-4429] ("#+end_src")
                    newLine [4429-4430]
                    keyword [4430-4441]
                      text [4430-4440] ("#+RESULTS:")
                      text [4440-4441] (" ")
                    newLine [4441-4442]
                    fixedWidth [4442-4466]
                      operator [4442-4444] (": ")
                      text [4444-4466] ("But this code will be!")
                    newLine [4466-4467]
                  section [4379-4527]
                    indent [4379-4391] ("            ")
                    text [4391-4399] ("(d nil))")
                    newLine [4399-4400]
                    indent [4400-4402] ("  ")
                    text [4402-4447] ("(message \\"This code never will be executed\\"))")
                    newLine [4447-4448]
                    newLine [4448-4449]
                    text [4449-4466] ("(when-let* ((b 4)")
                    newLine [4466-4467]
                    indent [4467-4479] ("            ")
                    text [4479-4488] ("(d \\"He\\"))")
                    newLine [4488-4489]
                    indent [4489-4491] ("  ")
                    text [4491-4526] ("(message \\"But this code will be!\\"))")
                    newLine [4526-4527]
                headline [4615-5111]
                    :level 3:
                  title [4615-4995]
                    operator [4615-4619] ("*** ")
                    text [4619-4632] ("Работа с char")
                    newLine [4632-4633]
                    text [4633-4674] ("Char в лиспе обозначается знаком вопроса.")
                    newLine [4674-4675]
                    text [4675-4720] ("Конвертация осуществляется с помощью функции ")
                    verbatim [4720-4728]
                      operator [4720-4721] ("=")
                      text [4721-4727] ("string")
                      operator [4727-4728] ("=")
                    text [4728-4784] (", либо если это список из символов то с помощью функции ")
                    verbatim [4784-4792]
                      operator [4784-4785] ("=")
                      text [4785-4791] ("concat")
                      operator [4791-4792] ("=")
                    newLine [4792-4793]
                    newLine [4793-4794]
                    srcBlock [4794-4855]
                      blockHeader [4794-4816]
                        keyword [4794-4816]
                          text [4794-4805] ("#+begin_src")
                          text [4805-4816] (" emacs-lisp")
                      newLine [4816-4817]
                      blockBody [4817-4845]
                        text [4817-4845] ("\\n(let ((my-awesome-char ?Q))")
                      newLine [4845-4846]
                      blockFooter [4846-4855]
                        keyword [4846-4855]
                          text [4846-4855] ("#+end_src")
                    newLine [4855-4856]
                    keyword [4856-4867]
                      text [4856-4866] ("#+RESULTS:")
                      text [4866-4867] (" ")
                    newLine [4867-4868]
                    fixedWidth [4868-4875]
                      operator [4868-4870] (": ")
                      text [4870-4875] ("World")
                    newLine [4875-4876]
                    text [4876-4915] ("С помощью символов можно сделать repeat")
                    newLine [4915-4916]
                    newLine [4916-4917]
                    srcBlock [4917-4969]
                      blockHeader [4917-4939]
                        keyword [4917-4939]
                          text [4917-4928] ("#+begin_src")
                          text [4928-4939] (" emacs-lisp")
                      newLine [4939-4940]
                      blockBody [4940-4959]
                        text [4940-4959] ("(make-string 10 ?|)")
                      newLine [4959-4960]
                      blockFooter [4960-4969]
                        keyword [4960-4969]
                          text [4960-4969] ("#+end_src")
                    newLine [4969-4970]
                    keyword [4970-4981]
                      text [4970-4980] ("#+RESULTS:")
                      text [4980-4981] (" ")
                    newLine [4981-4982]
                    fixedWidth [4982-4994]
                      operator [4982-4984] (": ")
                      text [4984-4994] ("||||||||||")
                    newLine [4994-4995]
                  section [4633-4749]
                    indent [4633-4647] ("              ")
                    text [4647-4696] ("(message (string my-awesome-char ?H ?e ?e ?l ?o))")
                    newLine [4696-4697]
                    indent [4697-4711] ("              ")
                    text [4711-4748] ("(message (concat '(?W ?o ?r ?l ?d))))")
                    newLine [4748-4749]
                headline [5111-5259]
                    :level 3:
                  title [5111-5259]
                    operator [5111-5115] ("*** ")
                    text [5115-5133] ("Работа со строками")
                    newLine [5133-5134]
                    bold [5134-5157]
                      operator [5134-5135] ("*")
                      text [5135-5156] ("Форматирование строки")
                      operator [5156-5157] ("*")
                    newLine [5157-5158]
                    newLine [5158-5159]
                    srcBlock [5159-5231]
                      blockHeader [5159-5181]
                        keyword [5159-5181]
                          text [5159-5170] ("#+begin_src")
                          text [5170-5181] (" emacs-lisp")
                      newLine [5181-5182]
                      blockBody [5182-5221]
                        text [5182-5221] ("(message (format \\"Hello %s\\n\\" \\"World?\\"))")
                      newLine [5221-5222]
                      blockFooter [5222-5231]
                        keyword [5222-5231]
                          text [5222-5231] ("#+end_src")
                    newLine [5231-5232]
                    keyword [5232-5243]
                      text [5232-5242] ("#+RESULTS:")
                      text [5242-5243] (" ")
                    newLine [5243-5244]
                    fixedWidth [5244-5258]
                      operator [5244-5246] (": ")
                      text [5246-5258] ("Hello World?")
                    newLine [5258-5259]
                  section [5134-5134]
                headline [5259-7740]
                    :level 3:
                  title [5259-5558]
                    operator [5259-5263] ("*** ")
                    text [5263-5269] ("Списки")
                    newLine [5269-5270]
                    text [5270-5433] ("Списки \\"экранируются\\" (на самом деле это не экранирование, т.к. все в лиспе функция это просто указатель на то что это не нужно исполнять, называется это evaluate ")
                    italic [5433-5461]
                      operator [5433-5434] ("/")
                      text [5434-5460] ("но это конечно же не точно")
                      operator [5460-5461] ("/")
                    text [5461-5481] (") с помощью симола '")
                    newLine [5481-5482]
                    newLine [5482-5483]
                    srcBlock [5483-5557]
                      blockHeader [5483-5505]
                        keyword [5483-5505]
                          text [5483-5494] ("#+begin_src")
                          text [5494-5505] (" emacs-lisp")
                      newLine [5505-5506]
                      blockBody [5506-5547]
                        text [5506-5547] ("(setq my-first-list '(\\"Foo\\" \\"Baz\\" \\"Qwe\\"))")
                      newLine [5547-5548]
                      blockFooter [5548-5557]
                        keyword [5548-5557]
                          text [5548-5557] ("#+end_src")
                    newLine [5557-5558]
                  section [5270-7452]
                    headline [5270-5370]
                        :level 4:
                      title [5270-5370]
                        operator [5270-5275] ("**** ")
                        text [5275-5298] ("Получить первый элемент")
                        newLine [5298-5299]
                        srcBlock [5299-5351]
                          blockHeader [5299-5321]
                            keyword [5299-5321]
                              text [5299-5310] ("#+begin_src")
                              text [5310-5321] (" emacs-lisp")
                          newLine [5321-5322]
                          blockBody [5322-5341]
                            text [5322-5341] ("(car my-first-list)")
                          newLine [5341-5342]
                          blockFooter [5342-5351]
                            keyword [5342-5351]
                              text [5342-5351] ("#+end_src")
                        newLine [5351-5352]
                        keyword [5352-5363]
                          text [5352-5362] ("#+RESULTS:")
                          text [5362-5363] (" ")
                        newLine [5363-5364]
                        fixedWidth [5364-5369]
                          operator [5364-5366] (": ")
                          text [5366-5369] ("Foo")
                        newLine [5369-5370]
                      section [5299-5299]
                    headline [5370-5466]
                        :level 4:
                      title [5370-5466]
                        operator [5370-5375] ("**** ")
                        text [5375-5412] ("Получить все кроме первого элемента..")
                        newLine [5412-5413]
                        srcBlock [5413-5465]
                          blockHeader [5413-5435]
                            keyword [5413-5435]
                              text [5413-5424] ("#+begin_src")
                              text [5424-5435] (" emacs-lisp")
                          newLine [5435-5436]
                          blockBody [5436-5455]
                            text [5436-5455] ("(cdr my-first-list)")
                          newLine [5455-5456]
                          blockFooter [5456-5465]
                            keyword [5456-5465]
                              text [5456-5465] ("#+end_src")
                        newLine [5465-5466]
                      section [5413-5413]
                    headline [5466-5958]
                        :level 4:
                      title [5466-5958]
                        operator [5466-5471] ("**** ")
                        text [5471-5496] ("Добавить элемент в список")
                        newLine [5496-5497]
                        text [5497-5587] ("Push мутирует список. Не выглядит как нечто функциональное, но возможно я что-то не понял.")
                        newLine [5587-5588]
                        newLine [5588-5589]
                        srcBlock [5589-5705]
                          blockHeader [5589-5611]
                            keyword [5589-5611]
                              text [5589-5600] ("#+begin_src")
                              text [5600-5611] (" emacs-lisp")
                          newLine [5611-5612]
                          blockBody [5612-5695]
                            text [5612-5695] ("(setq my-first-list '())\\n(push \\"Lalalend\\" my-first-list)\\n(push \\"Hey\\" my-first-list)")
                          newLine [5695-5696]
                          blockFooter [5696-5705]
                            keyword [5696-5705]
                              text [5696-5705] ("#+end_src")
                        newLine [5705-5706]
                        table [5706-5725]
                          tableRow [5706-5724]
                            operator [5706-5707] ("|")
                            tableCell [5707-5712]
                              text [5707-5712] (" Hey ")
                            operator [5712-5713] ("|")
                            tableCell [5713-5723]
                              text [5713-5723] (" Lalalend ")
                            operator [5723-5724] ("|")
                          newLine [5724-5725]
                        text [5725-5777] ("Ну или так (последний аргумент t - добавить в конец)")
                        newLine [5777-5778]
                        newLine [5778-5779]
                        srcBlock [5779-5923]
                          blockHeader [5779-5801]
                            keyword [5779-5801]
                              text [5779-5790] ("#+begin_src")
                              text [5790-5801] (" emacs-lisp")
                          newLine [5801-5802]
                          blockBody [5802-5913]
                            text [5802-5913] ("(setq my-test-2-list '(\\"qweqweqwe\\" \\"123\\"))\\n(add-to-list 'my-test-2-list \\"qwe\\" t)\\n\\n(message \\"%s\\" my-test-2-list)")
                          newLine [5913-5914]
                          blockFooter [5914-5923]
                            keyword [5914-5923]
                              text [5914-5923] ("#+end_src")
                        newLine [5923-5924]
                        keyword [5924-5935]
                          text [5924-5934] ("#+RESULTS:")
                          text [5934-5935] (" ")
                        newLine [5935-5936]
                        fixedWidth [5936-5957]
                          operator [5936-5938] (": ")
                          text [5938-5957] ("(qweqweqwe 123 qwe)")
                        newLine [5957-5958]
                      section [5497-5497]
                    headline [5958-6160]
                        :level 4:
                      title [5958-6160]
                        operator [5958-5963] ("**** ")
                        text [5963-5980] ("Слияние 2 списков")
                        newLine [5980-5981]
                        srcBlock [5981-6129]
                          blockHeader [5981-6003]
                            keyword [5981-6003]
                              text [5981-5992] ("#+begin_src")
                              text [5992-6003] (" emacs-lisp")
                          newLine [6003-6004]
                          blockBody [6004-6119]
                            text [6004-6119] ("(setq my-first-list '(?q ?b ?c))\\n(setq my-first-list (append my-first-list (list ?t)))\\n(message \\"%s\\" my-first-list)")
                          newLine [6119-6120]
                          blockFooter [6120-6129]
                            keyword [6120-6129]
                              text [6120-6129] ("#+end_src")
                        newLine [6129-6130]
                        keyword [6130-6141]
                          text [6130-6140] ("#+RESULTS:")
                          text [6140-6141] (" ")
                        newLine [6141-6142]
                        fixedWidth [6142-6159]
                          operator [6142-6144] (": ")
                          text [6144-6159] ("(113 98 99 116)")
                        newLine [6159-6160]
                      section [5981-5981]
                    headline [6160-6371]
                        :level 4:
                      title [6160-6278]
                        operator [6160-6165] ("**** ")
                        text [6165-6168] ("Map")
                        newLine [6168-6169]
                        text [6169-6183] ("На самом деле ")
                        verbatim [6183-6191]
                          operator [6183-6184] ("=")
                          text [6184-6190] ("mapcar")
                          operator [6190-6191] ("=")
                        text [6191-6192] (" ")
                        crossed [6192-6243]
                          operator [6192-6193] ("+")
                          text [6193-6242] ("(возможно создатель языка хотел иметь машину...).")
                          operator [6242-6243] ("+")
                        newLine [6243-6244]
                        newLine [6244-6245]
                        srcBlock [6245-6277]
                          blockHeader [6245-6267]
                            keyword [6245-6267]
                              text [6245-6256] ("#+begin_src")
                              text [6256-6267] (" emacs-lisp")
                          newLine [6267-6268]
                          blockFooter [6268-6277]
                            keyword [6268-6277]
                              text [6268-6277] ("#+end_src")
                        newLine [6277-6278]
                      section [6169-6262]
                        indent [6169-6171] ("  ")
                        text [6171-6193] ("(defun greeting (name)")
                        newLine [6193-6194]
                        indent [6194-6198] ("    ")
                        text [6198-6223] ("(format \\"Hello %s\\" name))")
                        newLine [6223-6224]
                        indent [6224-6226] ("  ")
                        newLine [6226-6227]
                        indent [6227-6229] ("  ")
                        text [6229-6261] ("(mapcar 'greeting my-first-list)")
                        newLine [6261-6262]
                    headline [6371-6631]
                        :level 4:
                      title [6371-6548]
                        operator [6371-6376] ("**** ")
                        text [6376-6383] ("forEach")
                        newLine [6383-6384]
                        verbatim [6384-6391]
                          operator [6384-6385] ("=")
                          text [6385-6390] ("mpcar")
                          operator [6390-6391] ("=")
                        text [6391-6468] (" создает новый список, можно просто итерироваться по записям с помощь. dolist")
                        newLine [6468-6469]
                        newLine [6469-6470]
                        srcBlock [6470-6518]
                          blockHeader [6470-6492]
                            keyword [6470-6492]
                              text [6470-6481] ("#+begin_src")
                              text [6481-6492] (" emacs-lisp")
                          newLine [6492-6493]
                          blockBody [6493-6508]
                            text [6493-6508] ("(let* ((v \\"\\"))\\n")
                          newLine [6508-6509]
                          blockFooter [6509-6518]
                            keyword [6509-6518]
                              text [6509-6518] ("#+end_src")
                        newLine [6518-6519]
                        keyword [6519-6530]
                          text [6519-6529] ("#+RESULTS:")
                          text [6529-6530] (" ")
                        newLine [6530-6531]
                        fixedWidth [6531-6547]
                          operator [6531-6533] (": ")
                          text [6533-6547] (" one two three")
                        newLine [6547-6548]
                      section [6384-6467]
                        indent [6384-6386] ("  ")
                        text [6386-6420] ("(dolist (p '(\\"one\\" \\"two\\" \\"three\\"))")
                        newLine [6420-6421]
                        indent [6421-6425] ("    ")
                        text [6425-6451] ("(setq v (concat v \\" \\" p)))")
                        newLine [6451-6452]
                        indent [6452-6454] ("  ")
                        text [6454-6466] ("(message v))")
                        newLine [6466-6467]
                    headline [6631-6747]
                        :level 4:
                      title [6631-6747]
                        operator [6631-6636] ("**** ")
                        text [6636-6670] ("Проверить есть ли элемент в списке")
                        newLine [6670-6671]
                        srcBlock [6671-6738]
                          blockHeader [6671-6693]
                            keyword [6671-6693]
                              text [6671-6682] ("#+begin_src")
                              text [6682-6693] (" emacs-lisp")
                          newLine [6693-6694]
                          blockBody [6694-6728]
                            text [6694-6728] ("(member \\"123\\" '(1233 \\"qwe\\" \\"123\\"))")
                          newLine [6728-6729]
                          blockFooter [6729-6738]
                            keyword [6729-6738]
                              text [6729-6738] ("#+end_src")
                        newLine [6738-6739]
                        table [6739-6747]
                          tableRow [6739-6746]
                            operator [6739-6740] ("|")
                            tableCell [6740-6745]
                              text [6740-6745] (" 123 ")
                            operator [6745-6746] ("|")
                          newLine [6746-6747]
                      section [6671-6671]
                    headline [6747-7197]
                        :level 4:
                      title [6747-7197]
                        operator [6747-6752] ("**** ")
                        text [6752-6792] ("Перезаписать элемент в списке по индексу")
                        newLine [6792-6793]
                        srcBlock [6793-6950]
                          blockHeader [6793-6815]
                            keyword [6793-6815]
                              text [6793-6804] ("#+begin_src")
                              text [6804-6815] (" emacs-lisp")
                          newLine [6815-6816]
                          blockBody [6816-6940]
                            text [6816-6940] ("(setq my-test-list '((\\"qwe\\" . 1) (\\"be\\" . 2)))\\n(setcdr (assoc \\"qwe\\" my-test-list) \\"asdlkajsdakd\\")\\n(message \\"%s\\" my-test-list)")
                          newLine [6940-6941]
                          blockFooter [6941-6950]
                            keyword [6941-6950]
                              text [6941-6950] ("#+end_src")
                        newLine [6950-6951]
                        keyword [6951-6962]
                          text [6951-6961] ("#+RESULTS:")
                          text [6961-6962] (" ")
                        newLine [6962-6963]
                        fixedWidth [6963-6996]
                          operator [6963-6965] (": ")
                          text [6965-6996] ("((qwe . asdlkajsdakd) (be . 2))")
                        newLine [6996-6997]
                        newLine [6997-6998]
                        text [6998-7037] ("А что если этого элемента нет в списке?")
                        newLine [7037-7038]
                        srcBlock [7038-7183]
                          blockHeader [7038-7060]
                            keyword [7038-7060]
                              text [7038-7049] ("#+begin_src")
                              text [7049-7060] (" emacs-lisp")
                          newLine [7060-7061]
                          blockBody [7061-7173]
                            text [7061-7173] ("(setq my-test-list '((\\"be\\" . 2)))\\n(setcdr (assoc \\"qwe\\" my-test-list) \\"asdlkajsdakd\\")\\n(message \\"%s\\" my-test-list)")
                          newLine [7173-7174]
                          blockFooter [7174-7183]
                            keyword [7174-7183]
                              text [7174-7183] ("#+end_src")
                        newLine [7183-7184]
                        newLine [7184-7185]
                        text [7185-7196] ("Не работает")
                        newLine [7196-7197]
                      section [6793-6793]
                    headline [7197-7452]
                        :level 4:
                      title [7197-7319]
                        operator [7197-7202] ("**** ")
                        text [7202-7227] ("Удалить элемент из списка")
                        newLine [7227-7228]
                        srcBlock [7228-7316]
                          blockHeader [7228-7266]
                            keyword [7228-7251]
                              text [7228-7239] ("#+BEGIN_SRC")
                              text [7239-7251] (" emacs-lisp ")
                            blockProperty [7251-7266]
                              text [7251-7259] (":results")
                              text [7259-7266] (" silent")
                          newLine [7266-7267]
                          blockBody [7267-7306]
                            text [7267-7306] ("ELISP> (setq list1 '(alpha beta gamma))")
                          newLine [7306-7307]
                          blockFooter [7307-7316]
                            keyword [7307-7316]
                              text [7307-7316] ("#+END_SRC")
                        newLine [7316-7317]
                        newLine [7317-7318]
                        newLine [7318-7319]
                      section [7228-7361]
                        indent [7228-7229] (" ")
                        text [7229-7247] ("(alpha beta gamma)")
                        newLine [7247-7248]
                        indent [7248-7249] (" ")
                        newLine [7249-7250]
                        indent [7250-7251] (" ")
                        text [7251-7291] ("ELISP> (setq list2 (delete 'beta list1))")
                        newLine [7291-7292]
                        indent [7292-7293] (" ")
                        text [7293-7306] ("(alpha gamma)")
                        newLine [7306-7307]
                        indent [7307-7308] (" ")
                        newLine [7308-7309]
                        indent [7309-7310] (" ")
                        text [7310-7351] ("ELISP> (setq list3 (delete 'alpha list1))")
                        newLine [7351-7352]
                        indent [7352-7353] (" ")
                        text [7353-7360] ("(gamma)")
                        newLine [7360-7361]
                headline [7740-9953]
                    :level 3:
                  title [7740-7766]
                    operator [7740-7744] ("*** ")
                    text [7744-7765] ("Ассоциативные массивы")
                    newLine [7765-7766]
                  section [7766-9953]
                    headline [7766-8094]
                        :level 4:
                      title [7766-8094]
                        operator [7766-7771] ("**** ")
                        text [7771-7781] ("Объявление")
                        newLine [7781-7782]
                        srcBlock [7782-7850]
                          blockHeader [7782-7804]
                            keyword [7782-7804]
                              text [7782-7793] ("#+begin_src")
                              text [7793-7804] (" emacs-lisp")
                          newLine [7804-7805]
                          blockBody [7805-7840]
                            text [7805-7840] ("(setq trees '((a . 1) (b . \\"qwe\\")))")
                          newLine [7840-7841]
                          blockFooter [7841-7850]
                            keyword [7841-7850]
                              text [7841-7850] ("#+end_src")
                        newLine [7850-7851]
                        keyword [7851-7862]
                          text [7851-7861] ("#+RESULTS:")
                          text [7861-7862] (" ")
                        newLine [7862-7863]
                        fixedWidth [7863-7884]
                          operator [7863-7865] (": ")
                          text [7865-7884] ("((a . 1) (b . qwe))")
                        newLine [7884-7885]
                        newLine [7885-7886]
                        text [7886-7928] ("При чем точка нужна для специального типа ")
                        italic [7928-7937]
                          operator [7928-7929] ("/")
                          text [7929-7936] ("symbols")
                          operator [7936-7937] ("/")
                        text [7937-7994] (". Если работает с реальными значениями то можно и без нее")
                        newLine [7994-7995]
                        newLine [7995-7996]
                        srcBlock [7996-8093]
                          blockHeader [7996-8018]
                            keyword [7996-8018]
                              text [7996-8007] ("#+begin_src")
                              text [8007-8018] (" emacs-lisp")
                          newLine [8018-8019]
                          blockBody [8019-8083]
                            text [8019-8083] ("(setq another-hashmap '((\\"a\\" \\"First elem\\") (\\"b\\" \\"Second elem\\")))")
                          newLine [8083-8084]
                          blockFooter [8084-8093]
                            keyword [8084-8093]
                              text [8084-8093] ("#+end_src")
                        newLine [8093-8094]
                      section [7782-7782]
                    headline [8094-8392]
                        :level 4:
                      title [8094-8392]
                        operator [8094-8099] ("**** ")
                        text [8099-8124] ("Получить элемент по ключу")
                        newLine [8124-8125]
                        srcBlock [8125-8189]
                          blockHeader [8125-8147]
                            keyword [8125-8147]
                              text [8125-8136] ("#+begin_src")
                              text [8136-8147] (" emacs-lisp")
                          newLine [8147-8148]
                          blockBody [8148-8179]
                            text [8148-8179] ("(message \\"%s\\" (assoc 'a trees))")
                          newLine [8179-8180]
                          blockFooter [8180-8189]
                            keyword [8180-8189]
                              text [8180-8189] ("#+end_src")
                        newLine [8189-8190]
                        text [8190-8298] ("Ну и конечно возвращает оно кортеж..а чтобы получить элемент нужно использовать уже известную нам функцию - ")
                        verbatim [8298-8303]
                          operator [8298-8299] ("=")
                          text [8299-8302] ("cdr")
                          operator [8302-8303] ("=")
                        newLine [8303-8304]
                        newLine [8304-8305]
                        srcBlock [8305-8375]
                          blockHeader [8305-8327]
                            keyword [8305-8327]
                              text [8305-8316] ("#+begin_src")
                              text [8316-8327] (" emacs-lisp")
                          newLine [8327-8328]
                          blockBody [8328-8365]
                            text [8328-8365] ("(message \\"%s\\" (cdr (assoc 'a trees)))")
                          newLine [8365-8366]
                          blockFooter [8366-8375]
                            keyword [8366-8375]
                              text [8366-8375] ("#+end_src")
                        newLine [8375-8376]
                        keyword [8376-8387]
                          text [8376-8386] ("#+RESULTS:")
                          text [8386-8387] (" ")
                        newLine [8387-8388]
                        fixedWidth [8388-8391]
                          operator [8388-8390] (": ")
                          text [8390-8391] ("1")
                        newLine [8391-8392]
                      section [8125-8125]
                    headline [8392-8718]
                        :level 4:
                      title [8392-8718]
                        operator [8392-8397] ("**** ")
                        text [8397-8425] ("Получить элемент по значению")
                        newLine [8425-8426]
                        srcBlock [8426-8494]
                          blockHeader [8426-8448]
                            keyword [8426-8448]
                              text [8426-8437] ("#+begin_src")
                              text [8437-8448] (" emacs-lisp")
                          newLine [8448-8449]
                          blockBody [8449-8484]
                            text [8449-8484] ("(message \\"%s\\" (rassoc \\"qwe\\" trees))")
                          newLine [8484-8485]
                          blockFooter [8485-8494]
                            keyword [8485-8494]
                              text [8485-8494] ("#+end_src")
                        newLine [8494-8495]
                        text [8495-8504] ("При этом ")
                        italic [8504-8512]
                          operator [8504-8505] ("/")
                          text [8505-8511] ("rassoc")
                          operator [8511-8512] ("/")
                        text [8512-8553] (" работает и для строк и для чисел, а вот ")
                        italic [8553-8560]
                          operator [8553-8554] ("/")
                          text [8554-8559] ("rassq")
                          operator [8559-8560] ("/")
                        text [8560-8577] (" только для чисел")
                        newLine [8577-8578]
                        newLine [8578-8579]
                        srcBlock [8579-8695]
                          blockHeader [8579-8601]
                            keyword [8579-8601]
                              text [8579-8590] ("#+begin_src")
                              text [8590-8601] (" emacs-lisp")
                          newLine [8601-8602]
                          blockBody [8602-8685]
                            text [8602-8685] ("(message \\"%s\\" (rassq \\"qwe\\" trees)) ;; nil\\n(message \\"%s\\" (rassq 1 trees)) ;; (a . 1)")
                          newLine [8685-8686]
                          blockFooter [8686-8695]
                            keyword [8686-8695]
                              text [8686-8695] ("#+end_src")
                        newLine [8695-8696]
                        keyword [8696-8707]
                          text [8696-8706] ("#+RESULTS:")
                          text [8706-8707] (" ")
                        newLine [8707-8708]
                        fixedWidth [8708-8717]
                          operator [8708-8710] (": ")
                          text [8710-8717] ("(a . 1)")
                        newLine [8717-8718]
                      section [8426-8426]
                    headline [8718-9052]
                        :level 4:
                      title [8718-8846]
                        operator [8718-8723] ("**** ")
                        text [8723-8739] ("Копирование мапы")
                        newLine [8739-8740]
                        srcBlock [8740-8772]
                          blockHeader [8740-8762]
                            keyword [8740-8762]
                              text [8740-8751] ("#+begin_src")
                              text [8751-8762] (" emacs-lisp")
                          newLine [8762-8763]
                          blockFooter [8763-8772]
                            keyword [8763-8772]
                              text [8763-8772] ("#+end_src")
                        newLine [8772-8773]
                        keyword [8773-8784]
                          text [8773-8783] ("#+RESULTS:")
                          text [8783-8784] (" ")
                        newLine [8784-8785]
                        fixedWidth [8785-8845]
                          operator [8785-8787] (": ")
                          text [8787-8845] ("((2 Austrian Pine Red Pine) (3 Pitch Pine) (5 White Pine))")
                        newLine [8845-8846]
                      section [8740-8946]
                        indent [8740-8742] ("  ")
                        text [8742-8767] ("(setq needles-per-cluster")
                        newLine [8767-8768]
                        indent [8768-8776] ("        ")
                        text [8776-8812] ("'((2 . (\\"Austrian Pine\\" \\"Red Pine\\"))")
                        newLine [8812-8813]
                        indent [8813-8823] ("          ")
                        text [8823-8843] ("(3 . (\\"Pitch Pine\\"))")
                        newLine [8843-8844]
                        indent [8844-8854] ("          ")
                        text [8854-8876] ("(5 . (\\"White Pine\\"))))")
                        newLine [8876-8877]
                        indent [8877-8879] ("  ")
                        text [8879-8923] ("(setq copy (copy-alist needles-per-cluster))")
                        newLine [8923-8924]
                        indent [8924-8926] ("  ")
                        text [8926-8945] ("(message \\"%s\\" copy)")
                        newLine [8945-8946]
                    headline [9052-9456]
                        :level 4:
                      title [9052-9196]
                        operator [9052-9057] ("**** ")
                        text [9057-9087] ("Удаление всех записей по ключу")
                        newLine [9087-9088]
                        srcBlock [9088-9120]
                          blockHeader [9088-9110]
                            keyword [9088-9110]
                              text [9088-9099] ("#+begin_src")
                              text [9099-9110] (" emacs-lisp")
                          newLine [9110-9111]
                          blockFooter [9111-9120]
                            keyword [9111-9120]
                              text [9111-9120] ("#+end_src")
                        newLine [9120-9121]
                        keyword [9121-9132]
                          text [9121-9131] ("#+RESULTS:")
                          text [9131-9132] (" ")
                        newLine [9132-9133]
                        fixedWidth [9133-9168]
                          operator [9133-9135] (": ")
                          text [9135-9168] ("alist: ((foo 1) (bar 2) (lose 4))")
                        newLine [9168-9169]
                        fixedWidth [9169-9195]
                          operator [9169-9171] (": ")
                          text [9171-9195] (" new: ((bar 2) (lose 4))")
                        newLine [9195-9196]
                      section [9088-9348]
                        indent [9088-9090] ("  ")
                        text [9090-9146] ("(setq alist (list '(foo 1) '(bar 2) '(foo 3) '(lose 4)))")
                        newLine [9146-9147]
                        indent [9147-9149] ("  ")
                        text [9149-9223] ("(setq new-alist (assq-delete-all 'foo alist)) ;; Возвращает новое значение")
                        newLine [9223-9224]
                        indent [9224-9226] ("  ")
                        text [9226-9250] ("(message \\"%s\\" new-alist)")
                        newLine [9250-9251]
                        indent [9251-9253] ("  ")
                        text [9253-9288] ("(message (concat (format \\"alist: %s")
                        newLine [9288-9289]
                        text [9289-9297] ("\\" alist)")
                        newLine [9297-9298]
                        indent [9298-9317] ("                   ")
                        text [9317-9347] ("(format \\"new: %s\\" new-alist)))")
                        newLine [9347-9348]
                    headline [9456-9953]
                        :level 4:
                      title [9456-9639]
                        operator [9456-9461] ("**** ")
                        text [9461-9489] ("Удаление записей по значению")
                        newLine [9489-9490]
                        srcBlock [9490-9522]
                          blockHeader [9490-9512]
                            keyword [9490-9512]
                              text [9490-9501] ("#+begin_src")
                              text [9501-9512] (" emacs-lisp")
                          newLine [9512-9513]
                          blockFooter [9513-9522]
                            keyword [9513-9522]
                              text [9513-9522] ("#+end_src")
                        newLine [9522-9523]
                        keyword [9523-9534]
                          text [9523-9533] ("#+RESULTS:")
                          text [9533-9534] (" ")
                        newLine [9534-9535]
                        fixedWidth [9535-9587]
                          operator [9535-9537] (": ")
                          text [9537-9587] ("alist: ((foo . first) (bar . second) (qwe . five))")
                        newLine [9587-9588]
                        fixedWidth [9588-9638]
                          operator [9588-9590] (": ")
                          text [9590-9638] ("new: ((foo . first) (bar . second) (qwe . five))")
                        newLine [9638-9639]
                      section [9490-9804]
                        indent [9490-9492] ("  ")
                        text [9492-9565] ("(setq alist2 '((foo . first) (bar . second) (foo2 . third) (qwe . five)))")
                        newLine [9565-9566]
                        indent [9566-9568] ("  ")
                        text [9568-9638] ("(setq new-alist (rassq-delete-all 'third alist2)) ;; меняет значение ?")
                        newLine [9638-9639]
                        indent [9639-9641] ("  ")
                        text [9641-9665] ("(message \\"%s\\" new-alist)")
                        newLine [9665-9666]
                        indent [9666-9668] ("  ")
                        text [9668-9703] ("(message (concat (format \\"alist: %s")
                        newLine [9703-9704]
                        text [9704-9713] ("\\" alist2)")
                        newLine [9713-9714]
                        indent [9714-9733] ("                   ")
                        text [9733-9763] ("(format \\"new: %s\\" new-alist)))")
                        newLine [9763-9764]
                        indent [9764-9766] ("  ")
                        text [9766-9803] (";; (message \\"%s\\" (rassq 'foo alist2))")
                        newLine [9803-9804]
                headline [9953-10621]
                    :level 3:
                  title [9953-10099]
                    operator [9953-9957] ("*** ")
                    text [9957-9963] ("Хешмап")
                    newLine [9963-9964]
                    link [9964-10036]
                      operator [9964-9965] ("[")
                      linkUrl [9965-10021]
                        operator [9965-9966] ("[")
                        text [9966-10020] ("htest-varp://ergoemacs.org/emacs/elisp_hash_table.html")
                        operator [10020-10021] ("]")
                      linkName [10021-10035]
                        operator [10021-10022] ("[")
                        text [10022-10034] ("Документация")
                        operator [10034-10035] ("]")
                      operator [10035-10036] ("]")
                    newLine [10036-10037]
                    newLine [10037-10038]
                    srcBlock [10038-10070]
                      blockHeader [10038-10060]
                        keyword [10038-10060]
                          text [10038-10049] ("#+begin_src")
                          text [10049-10060] (" emacs-lisp")
                      newLine [10060-10061]
                      blockFooter [10061-10070]
                        keyword [10061-10070]
                          text [10061-10070] ("#+end_src")
                    newLine [10070-10071]
                    keyword [10071-10082]
                      text [10071-10081] ("#+RESULTS:")
                      text [10081-10082] (" ")
                    newLine [10082-10083]
                    fixedWidth [10083-10098]
                      operator [10083-10085] (": ")
                      text [10085-10098] ("ugly language")
                    newLine [10098-10099]
                  section [9964-10486]
                    indent [9964-9966] ("  ")
                    text [9966-9988] ("(setq my-first-map #s(")
                    newLine [9988-9989]
                    indent [9989-10013] ("                        ")
                    text [10013-10023] ("hash-table")
                    newLine [10023-10024]
                    indent [10024-10048] ("                        ")
                    text [10048-10055] ("size 10")
                    newLine [10055-10056]
                    indent [10056-10080] ("                        ")
                    text [10080-10090] ("test equal")
                    newLine [10090-10091]
                    indent [10091-10115] ("                        ")
                    text [10115-10121] ("data (")
                    newLine [10121-10122]
                    indent [10122-10152] ("                              ")
                    text [10152-10171] ("python-mode \\"spam!\\"")
                    newLine [10171-10172]
                    indent [10172-10202] ("                              ")
                    text [10202-10235] ("go-mode \\"booo!1 terrible pointer\\"")
                    newLine [10235-10236]
                    indent [10236-10266] ("                              ")
                    text [10266-10299] ("org-mode \\"amma fluffy feature ;p\\"")
                    newLine [10299-10300]
                    indent [10300-10330] ("                              ")
                    text [10330-10333] (")))")
                    newLine [10333-10334]
                    indent [10334-10336] ("  ")
                    text [10336-10383] ("(puthash 'js-mode \\"ugly language\\" my-first-map)")
                    newLine [10383-10384]
                    indent [10384-10386] ("  ")
                    text [10386-10436] ("(message \\"%s\\" (gethash 'python-mode my-first-map))")
                    newLine [10436-10437]
                    indent [10437-10439] ("  ")
                    text [10439-10485] ("(message \\"%s\\" (gethash 'js-mode my-first-map))")
                    newLine [10485-10486]
                headline [10621-10759]
                    :level 3:
                  title [10621-10759]
                    operator [10621-10625] ("*** ")
                    text [10625-10631] ("Символ")
                    newLine [10631-10632]
                    text [10632-10725] ("Тип данные соотвутствующий объекту с именем. Задаются символы с помощью 1 начальной кавычки. ")
                    verbatim [10725-10739]
                      operator [10725-10726] ("=")
                      text [10726-10738] ("'amma-symbol")
                      operator [10738-10739] ("=")
                    newLine [10739-10740]
                    newLine [10740-10741]
                    keyword [10741-10758]
                      text [10741-10758] ("#+CLOSE_{SPOILER}")
                    newLine [10758-10759]
                  section [10632-10632]
            headline [10793-14274]
                :level 2:
              title [10793-10844]
                operator [10793-10796] ("** ")
                text [10796-10803] ("Функции")
                newLine [10803-10804]
                keyword [10804-10841]
                  text [10804-10841] ("#+START_{SPOILER} Читать про функции ")
                text [10841-10842] (">")
                newLine [10842-10843]
                newLine [10843-10844]
              section [10804-14234]
                headline [10804-11265]
                    :level 3:
                  title [10804-11108]
                    operator [10804-10808] ("*** ")
                    text [10808-10826] ("Объявление функций")
                    newLine [10826-10827]
                    text [10827-10912] ("Функции принято комментировать, это позволяет смотреть документацию в автодополнении.")
                    newLine [10912-10913]
                    text [10913-10919] ("Вызов ")
                    verbatim [10919-10934]
                      operator [10919-10920] ("=")
                      text [10920-10933] ("(interactive)")
                      operator [10933-10934] ("=")
                    text [10934-11041] (" означается что функция публичная и может быть взывана пользователем напрямую, либо через сочетание клавиш.")
                    newLine [11041-11042]
                    newLine [11042-11043]
                    srcBlock [11043-11075]
                      blockHeader [11043-11065]
                        keyword [11043-11065]
                          text [11043-11054] ("#+begin_src")
                          text [11054-11065] (" emacs-lisp")
                      newLine [11065-11066]
                      blockFooter [11066-11075]
                        keyword [11066-11075]
                          text [11066-11075] ("#+end_src")
                    newLine [11075-11076]
                    keyword [11076-11087]
                      text [11076-11086] ("#+RESULTS:")
                      text [11086-11087] (" ")
                    newLine [11087-11088]
                    fixedWidth [11088-11107]
                      operator [11088-11090] (": ")
                      text [11090-11107] ("Hello, I’am Artur")
                    newLine [11107-11108]
                  section [10827-10984]
                    indent [10827-10829] ("  ")
                    text [10829-10851] ("(defun hello (my-name)")
                    newLine [10851-10852]
                    indent [10852-10856] ("    ")
                    text [10856-10899] ("\\"This function will say hello for MY-NAME.\\"")
                    newLine [10899-10900]
                    indent [10900-10904] ("    ")
                    text [10904-10917] ("(interactive)")
                    newLine [10917-10918]
                    indent [10918-10922] ("    ")
                    text [10922-10964] ("(message (concat \\"Hello, I'am \\" my-name)))")
                    newLine [10964-10965]
                    newLine [10965-10966]
                    indent [10966-10968] ("  ")
                    text [10968-10983] ("(hello \\"Artur\\")")
                    newLine [10983-10984]
                headline [11265-11546]
                    :level 3:
                  title [11265-11427]
                    operator [11265-11269] ("*** ")
                    text [11269-11291] ("Опицональные аргументы")
                    newLine [11291-11292]
                    srcBlock [11292-11396]
                      blockHeader [11292-11314]
                        keyword [11292-11314]
                          text [11292-11303] ("#+begin_src")
                          text [11303-11314] (" emacs-lisp")
                      newLine [11314-11315]
                      blockBody [11315-11386]
                        text [11315-11386] ("(defun my-super-optional-function (name &optional last-name patronymic)")
                      newLine [11386-11387]
                      blockFooter [11387-11396]
                        keyword [11387-11396]
                          text [11387-11396] ("#+end_src")
                    newLine [11396-11397]
                    keyword [11397-11408]
                      text [11397-11407] ("#+RESULTS:")
                      text [11407-11408] (" ")
                    newLine [11408-11409]
                    fixedWidth [11409-11426]
                      operator [11409-11411] (": ")
                      text [11411-11426] ("Artur  Proshkov")
                    newLine [11426-11427]
                  section [11292-11411]
                    indent [11292-11294] ("  ")
                    text [11294-11357] ("(message \\"%s %s %s\\" name (or last-name \\"\\") (or patronymic \\"\\")))")
                    newLine [11357-11358]
                    newLine [11358-11359]
                    text [11359-11410] ("(my-super-optional-function \\"Artur\\" nil \\"Proshkov\\")")
                    newLine [11410-11411]
                headline [11546-11863]
                    :level 3:
                  title [11546-11700]
                    operator [11546-11550] ("*** ")
                    text [11550-11571] ("Именованные аргументы")
                    newLine [11571-11572]
                    srcBlock [11572-11658]
                      blockHeader [11572-11594]
                        keyword [11572-11594]
                          text [11572-11583] ("#+begin_src")
                          text [11583-11594] (" emacs-lisp")
                      newLine [11594-11595]
                      blockBody [11595-11648]
                        text [11595-11648] ("(defun my-super-function-with-named-args (&rest args)")
                      newLine [11648-11649]
                      blockFooter [11649-11658]
                        keyword [11649-11658]
                          text [11649-11658] ("#+end_src")
                    newLine [11658-11659]
                    keyword [11659-11670]
                      text [11659-11669] ("#+RESULTS:")
                      text [11669-11670] (" ")
                    newLine [11670-11671]
                    fixedWidth [11671-11699]
                      operator [11671-11673] (": ")
                      text [11673-11699] ("Name One, middle name Dude")
                    newLine [11699-11700]
                  section [11572-11735]
                    indent [11572-11574] ("  ")
                    text [11574-11625] ("(message \\"Name %s, middle name %s\\" (plist-get args ")
                    blockProperty [11625-11648]
                      text [11625-11631] (":name)")
                      text [11631-11648] (" (plist-get args ")
                    operator [11648-11649] (":")
                    text [11649-11663] ("middle-name)))")
                    newLine [11663-11664]
                    newLine [11664-11665]
                    indent [11665-11667] ("  ")
                    text [11667-11702] ("(my-super-function-with-named-args ")
                    blockProperty [11702-11714]
                      text [11702-11707] (":name")
                      text [11707-11714] (" \\"One\\" ")
                    blockProperty [11714-11734]
                      text [11714-11726] (":middle-name")
                      text [11726-11734] (" \\"Dude\\")")
                    newLine [11734-11735]
                headline [11863-12047]
                    :level 3:
                  title [11863-12047]
                    operator [11863-11867] ("*** ")
                    text [11867-11873] ("Лямбды")
                    newLine [11873-11874]
                    crossed [11874-11931]
                      operator [11874-11875] ("+")
                      text [11875-11930] ("Очевидно, лямбды нужны чтобы код можно было хуже читать")
                      operator [11930-11931] ("+")
                    newLine [11931-11932]
                    newLine [11932-11933]
                    srcBlock [11933-12016]
                      blockHeader [11933-11955]
                        keyword [11933-11955]
                          text [11933-11944] ("#+begin_src")
                          text [11944-11955] (" emacs-lisp")
                      newLine [11955-11956]
                      blockBody [11956-12006]
                        text [11956-12006] ("(funcall '(lambda () (message \\"I'am dirty func\\")))")
                      newLine [12006-12007]
                      blockFooter [12007-12016]
                        keyword [12007-12016]
                          text [12007-12016] ("#+end_src")
                    newLine [12016-12017]
                    keyword [12017-12028]
                      text [12017-12027] ("#+RESULTS:")
                      text [12027-12028] (" ")
                    newLine [12028-12029]
                    fixedWidth [12029-12046]
                      operator [12029-12031] (": ")
                      text [12031-12046] ("I’am dirty func")
                    newLine [12046-12047]
                  section [11874-11874]
                headline [12047-12624]
                    :level 3:
                  title [12047-12357]
                    operator [12047-12051] ("*** ")
                    text [12051-12057] ("Advice")
                    newLine [12057-12058]
                    text [12058-12160] ("Адвайсы это прокаченные декораторы. Могут быть вызваны как до так и после вызова оригинальной функции.")
                    newLine [12160-12161]
                    newLine [12161-12162]
                    srcBlock [12162-12218]
                      blockHeader [12162-12184]
                        keyword [12162-12184]
                          text [12162-12173] ("#+begin_src")
                          text [12173-12184] (" emacs-lisp")
                      newLine [12184-12185]
                      blockBody [12185-12208]
                        text [12185-12208] ("(defun my-increment (n)")
                      newLine [12208-12209]
                      blockFooter [12209-12218]
                        keyword [12209-12218]
                          text [12209-12218] ("#+end_src")
                    newLine [12218-12219]
                    keyword [12219-12230]
                      text [12219-12229] ("#+RESULTS:")
                      text [12229-12230] (" ")
                    newLine [12230-12231]
                    fixedWidth [12231-12235]
                      operator [12231-12233] (": ")
                      text [12233-12235] ("55")
                    newLine [12235-12236]
                    bold [12236-12278]
                      operator [12236-12237] ("*")
                      text [12237-12277] ("Пример адвайса после выполненеия функции")
                      operator [12277-12278] ("*")
                    newLine [12278-12279]
                    newLine [12279-12280]
                    srcBlock [12280-12335]
                      blockHeader [12280-12302]
                        keyword [12280-12302]
                          text [12280-12291] ("#+begin_src")
                          text [12291-12302] (" emacs-lisp")
                      newLine [12302-12303]
                      blockBody [12303-12325]
                        text [12303-12325] ("(defun my-first-func()")
                      newLine [12325-12326]
                      blockFooter [12326-12335]
                        keyword [12326-12335]
                          text [12326-12335] ("#+end_src")
                    newLine [12335-12336]
                    keyword [12336-12347]
                      text [12336-12346] ("#+RESULTS:")
                      text [12346-12347] (" ")
                    newLine [12347-12348]
                    fixedWidth [12348-12356]
                      operator [12348-12350] (": ")
                      text [12350-12356] ("qweqwe")
                    newLine [12356-12357]
                  section [12058-12325]
                    indent [12058-12060] ("  ")
                    text [12060-12068] ("(+ n 1))")
                    newLine [12068-12069]
                    newLine [12069-12070]
                    text [12070-12086] ("(defun mux-5 (n)")
                    newLine [12086-12087]
                    indent [12087-12089] ("  ")
                    text [12089-12097] ("(* n 5))")
                    newLine [12097-12098]
                    newLine [12098-12099]
                    text [12099-12125] ("(advice-add 'my-increment ")
                    blockProperty [12125-12148]
                      text [12125-12139] (":filter-return")
                      text [12139-12148] (" #'mux-5)")
                    newLine [12148-12149]
                    text [12149-12181] ("(message \\"%s\\" (my-increment 10))")
                    newLine [12181-12182]
                    indent [12182-12184] ("  ")
                    text [12184-12203] ("(message \\"qweqwe\\"))")
                    newLine [12203-12204]
                    text [12204-12219] ("(my-first-func)")
                    newLine [12219-12220]
                    text [12220-12235] ("(defun my-adv()")
                    newLine [12235-12236]
                    indent [12236-12238] ("  ")
                    text [12238-12264] ("(message \\"advice called\\"))")
                    newLine [12264-12265]
                    text [12265-12277] ("(advice-add ")
                    blockProperty [12277-12308]
                      text [12277-12283] (":after")
                      text [12283-12308] (" 'my-first-func #'my-adv)")
                    newLine [12308-12309]
                    text [12309-12324] ("(my-first-func)")
                    newLine [12324-12325]
                headline [12624-13831]
                    :level 3:
                  title [12624-13599]
                    operator [12624-12628] ("*** ")
                    text [12628-12649] ("Property list (plist)")
                    newLine [12649-12650]
                    bold [12650-12670]
                      operator [12650-12651] ("*")
                      text [12651-12669] ("Установка и запись")
                      operator [12669-12670] ("*")
                    newLine [12670-12671]
                    newLine [12671-12672]
                    srcBlock [12672-12896]
                      blockHeader [12672-12694]
                        keyword [12672-12694]
                          text [12672-12683] ("#+begin_src")
                          text [12683-12694] (" emacs-lisp")
                      newLine [12694-12695]
                      blockBody [12695-12886]
                        text [12695-12886] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n(message \\"enabled: %s, another prop: %s, type: %s\\" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop) (type-of my-plist))")
                      newLine [12886-12887]
                      blockFooter [12887-12896]
                        keyword [12887-12896]
                          text [12887-12896] ("#+end_src")
                    newLine [12896-12897]
                    keyword [12897-12908]
                      text [12897-12907] ("#+RESULTS:")
                      text [12907-12908] (" ")
                    newLine [12908-12909]
                    fixedWidth [12909-12952]
                      operator [12909-12911] (": ")
                      text [12911-12952] ("enabled: t, another prop: hey, type: cons")
                    newLine [12952-12953]
                    newLine [12953-12954]
                    bold [12954-12965]
                      operator [12954-12955] ("*")
                      text [12955-12964] ("Изменение")
                      operator [12964-12965] ("*")
                    newLine [12965-12966]
                    newLine [12966-12967]
                    srcBlock [12967-13220]
                      blockHeader [12967-12989]
                        keyword [12967-12989]
                          text [12967-12978] ("#+begin_src")
                          text [12978-12989] (" emacs-lisp")
                      newLine [12989-12990]
                      blockBody [12990-13210]
                        text [12990-13210] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n\\n(plist-put my-plist  :another-prop \\"Wow, i was changed\\")\\n(message \\"enabled: %s, another prop: %s\\" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop))")
                      newLine [13210-13211]
                      blockFooter [13211-13220]
                        keyword [13211-13220]
                          text [13211-13220] ("#+end_src")
                    newLine [13220-13221]
                    bold [13221-13240]
                      operator [13221-13222] ("*")
                      text [13222-13239] ("Итерация по plist")
                      operator [13239-13240] ("*")
                    newLine [13240-13241]
                    newLine [13241-13242]
                    srcBlock [13242-13387]
                      blockHeader [13242-13264]
                        keyword [13242-13264]
                          text [13242-13253] ("#+begin_src")
                          text [13253-13264] (" emacs-lisp")
                      newLine [13264-13265]
                      blockBody [13265-13377]
                        text [13265-13377] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n\\n(setq res \\"res: \\")\\n(loop for (k v) on my-plist by 'cddr do")
                      newLine [13377-13378]
                      blockFooter [13378-13387]
                        keyword [13378-13387]
                          text [13378-13387] ("#+end_src")
                    newLine [13387-13388]
                    bold [13388-13416]
                      operator [13388-13389] ("*")
                      text [13389-13415] ("Удаление элемента из plist")
                      operator [13415-13416] ("*")
                    newLine [13416-13417]
                    newLine [13417-13418]
                    srcBlock [13418-13549]
                      blockHeader [13418-13440]
                        keyword [13418-13440]
                          text [13418-13429] ("#+begin_src")
                          text [13429-13440] (" emacs-lisp")
                      newLine [13440-13441]
                      blockBody [13441-13539]
                        text [13441-13539] ("(setq test '(:hi \\"there\\" :by \\"man!\\"))\\n\\n(setq test (map-delete test :hi))\\n\\n(message \\"res: %s\\" test)")
                      newLine [13539-13540]
                      blockFooter [13540-13549]
                        keyword [13540-13549]
                          text [13540-13549] ("#+end_src")
                    newLine [13549-13550]
                    keyword [13550-13561]
                      text [13550-13560] ("#+RESULTS:")
                      text [13560-13561] (" ")
                    newLine [13561-13562]
                    fixedWidth [13562-13579]
                      operator [13562-13564] (": ")
                      text [13564-13579] ("res: (:by man!)")
                    newLine [13579-13580]
                    newLine [13580-13581]
                    keyword [13581-13598]
                      text [13581-13598] ("#+CLOSE_{SPOILER}")
                    newLine [13598-13599]
                  section [12650-12882]
                    indent [12650-12656] ("      ")
                    text [12656-12702] ("(setq res (concat res (format \\"%s - %s\\" k v) \\"")
                    newLine [12702-12703]
                    text [12703-12707] ("\\")))")
                    newLine [12707-12708]
                    newLine [12708-12709]
                    text [12709-12775] (";; (mapcar (lambda (k) (setq res (concat res (format \\"%s - \\" k ) \\"")
                    newLine [12775-12776]
                    text [12776-12790] ("\\"))) my-plist)")
                    newLine [12790-12791]
                    newLine [12791-12792]
                    newLine [12792-12793]
                    text [12793-12816] (";; (dolist (p my-plist)")
                    newLine [12816-12817]
                    text [12817-12861] (";;   (setq res (concat res (format \\"%s\\" p) \\"")
                    newLine [12861-12862]
                    text [12862-12866] ("\\")))")
                    newLine [12866-12867]
                    newLine [12867-12868]
                    text [12868-12881] ("(message res)")
                    newLine [12881-12882]
                headline [13831-14234]
                    :level 3:
                  title [13831-14234]
                    operator [13831-13835] ("*** ")
                    link [13835-13946]
                      operator [13835-13836] ("[")
                      linkUrl [13836-13918]
                        operator [13836-13837] ("[")
                        text [13837-13917] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html")
                        operator [13917-13918] ("]")
                      linkName [13918-13945]
                        operator [13918-13919] ("[")
                        text [13919-13944] ("Отложенный запуск функций")
                        operator [13944-13945] ("]")
                      operator [13945-13946] ("]")
                    newLine [13946-13947]
                    srcBlock [13947-14060]
                      blockHeader [13947-13969]
                        keyword [13947-13969]
                          text [13947-13958] ("#+begin_src")
                          text [13958-13969] (" emacs-lisp")
                      newLine [13969-13970]
                      blockBody [13970-14050]
                        text [13970-14050] ("(setq my-custom-timer (run-with-idle-timer 1 nil #'(lambda () (message \\"qwe\\"))))")
                      newLine [14050-14051]
                      blockFooter [14051-14060]
                        keyword [14051-14060]
                          text [14051-14060] ("#+end_src")
                    newLine [14060-14061]
                    keyword [14061-14072]
                      text [14061-14071] ("#+RESULTS:")
                      text [14071-14072] (" ")
                    newLine [14072-14073]
                    fixedWidth [14073-14133]
                      operator [14073-14075] (": ")
                      text [14075-14133] ("[nil 0 1 0 nil (lambda nil 'message \\"qwe\\") nil idle 0 nil]")
                    newLine [14133-14134]
                    newLine [14134-14135]
                    text [14135-14168] ("Отложенные функции можно отменять")
                    newLine [14168-14169]
                    newLine [14169-14170]
                    srcBlock [14170-14233]
                      blockHeader [14170-14192]
                        keyword [14170-14192]
                          text [14170-14181] ("#+begin_src")
                          text [14181-14192] (" emacs-lisp")
                      newLine [14192-14193]
                      blockBody [14193-14223]
                        text [14193-14223] ("(cancel-timer my-custom-timer)")
                      newLine [14223-14224]
                      blockFooter [14224-14233]
                        keyword [14224-14233]
                          text [14224-14233] ("#+end_src")
                    newLine [14233-14234]
                  section [13947-13947]
            headline [14274-15664]
                :level 2:
              title [14274-14422]
                operator [14274-14277] ("** ")
                text [14277-14286] ("Операторы")
                newLine [14286-14287]
                text [14287-14393] ("Орпеторы это точно такие же функции. Вынес в отдельную категорию т.к. в большинстве языков это инструкции.")
                newLine [14393-14394]
                keyword [14394-14419]
                  text [14394-14419] ("#+START_{SPOILER} Детали ")
                text [14419-14420] (">")
                newLine [14420-14421]
                newLine [14421-14422]
              section [14287-15529]
                headline [14287-14466]
                    :level 3:
                  title [14287-14418]
                    operator [14287-14291] ("*** ")
                    text [14291-14302] ("Switch case")
                    newLine [14302-14303]
                    srcBlock [14303-14399]
                      blockHeader [14303-14325]
                        keyword [14303-14325]
                          text [14303-14314] ("#+begin_src")
                          text [14314-14325] (" emacs-lisp")
                      newLine [14325-14326]
                      blockBody [14326-14389]
                        text [14326-14389] ("(setq test-var 'qwe)\\n(message \\"%s\\" (cond ((eq test-var 'q2e) 1)")
                      newLine [14389-14390]
                      blockFooter [14390-14399]
                        keyword [14390-14399]
                          text [14390-14399] ("#+end_src")
                    newLine [14399-14400]
                    keyword [14400-14411]
                      text [14400-14410] ("#+RESULTS:")
                      text [14410-14411] (" ")
                    newLine [14411-14412]
                    fixedWidth [14412-14417]
                      operator [14412-14414] (": ")
                      text [14414-14417] ("qwe")
                    newLine [14417-14418]
                  section [14303-14351]
                    indent [14303-14310] ("       ")
                    text [14310-14331] ("((eq test-var 'oe) 2)")
                    newLine [14331-14332]
                    indent [14332-14339] ("       ")
                    text [14339-14350] ("(t \\"qwe\\")))")
                    newLine [14350-14351]
                headline [14466-14643]
                    :level 3:
                  title [14466-14571]
                    operator [14466-14470] ("*** ")
                    text [14470-14475] ("While")
                    newLine [14475-14476]
                    srcBlock [14476-14553]
                      blockHeader [14476-14498]
                        keyword [14476-14498]
                          text [14476-14487] ("#+begin_src")
                          text [14487-14498] (" emacs-lisp")
                      newLine [14498-14499]
                      blockBody [14499-14543]
                        text [14499-14543] ("(setq my-counter 0)\\n(while (< my-counter 12)")
                      newLine [14543-14544]
                      blockFooter [14544-14553]
                        keyword [14544-14553]
                          text [14544-14553] ("#+end_src")
                    newLine [14553-14554]
                    keyword [14554-14565]
                      text [14554-14564] ("#+RESULTS:")
                      text [14564-14565] (" ")
                    newLine [14565-14566]
                    fixedWidth [14566-14570]
                      operator [14566-14568] (": ")
                      text [14568-14570] ("12")
                    newLine [14570-14571]
                  section [14476-14548]
                    indent [14476-14485] ("         ")
                    text [14485-14520] ("(setq my-counter (+ my-counter 1)))")
                    newLine [14520-14521]
                    newLine [14521-14522]
                    text [14522-14547] ("(message \\"%s\\" my-counter)")
                    newLine [14547-14548]
                headline [14643-15160]
                    :level 3:
                  title [14643-15005]
                    operator [14643-14647] ("*** ")
                    text [14647-14652] ("Catch")
                    newLine [14652-14653]
                    text [14653-14867] ("Просто вау, в фп есть try catch! Я действительно удивлен..даже в объектно ориетированых языках это вызывает проблемы..тем не менее..это 1 из вариантов прерываия цикла while (плохихи вариатов, как по мне, но все же)")
                    newLine [14867-14868]
                    newLine [14868-14869]
                    srcBlock [14869-15004]
                      blockHeader [14869-14891]
                        keyword [14869-14891]
                          text [14869-14880] ("#+begin_src")
                          text [14880-14891] (" emacs-lisp")
                      newLine [14891-14892]
                      blockBody [14892-14994]
                        text [14892-14994] ("(setq my-counter 0)\\n\\n\\n(message \\"What is the messafe from catch? Oh this is message: %s\\" (catch 'result")
                      newLine [14994-14995]
                      blockFooter [14995-15004]
                        keyword [14995-15004]
                          text [14995-15004] ("#+end_src")
                    newLine [15004-15005]
                  section [14653-14808]
                    indent [14653-14655] ("  ")
                    text [14655-14679] ("(while (< my-counter 22)")
                    newLine [14679-14680]
                    indent [14680-14684] ("    ")
                    text [14684-14718] ("(setq my-counter (+ my-counter 1))")
                    newLine [14718-14719]
                    indent [14719-14723] ("    ")
                    text [14723-14743] ("(if (> my-counter 5)")
                    newLine [14743-14744]
                    indent [14744-14752] ("        ")
                    text [14752-14799] ("(throw 'result \\"Amma result from catch block\\"))")
                    newLine [14799-14800]
                    indent [14800-14804] ("    ")
                    text [14804-14807] (")))")
                    newLine [14807-14808]
                headline [15160-15529]
                    :level 3:
                  title [15160-15349]
                    operator [15160-15164] ("*** ")
                    text [15164-15170] ("Return")
                    newLine [15170-15171]
                    text [15171-15182] ("Работает в ")
                    bold [15182-15195]
                      operator [15182-15183] ("*")
                      text [15183-15194] ("emacs 27.1+")
                      operator [15194-15195] ("*")
                    text [15195-15235] (". Позволяет прервать выполнение функции.")
                    newLine [15235-15236]
                    newLine [15236-15237]
                    srcBlock [15237-15313]
                      blockHeader [15237-15259]
                        keyword [15237-15259]
                          text [15237-15248] ("#+begin_src")
                          text [15248-15259] (" emacs-lisp")
                      newLine [15259-15260]
                      blockBody [15260-15303]
                        text [15260-15303] ("(setq my-counter 0)\\n(cl-defun my-iterator()")
                      newLine [15303-15304]
                      blockFooter [15304-15313]
                        keyword [15304-15313]
                          text [15304-15313] ("#+end_src")
                    newLine [15313-15314]
                    keyword [15314-15325]
                      text [15314-15324] ("#+RESULTS:")
                      text [15324-15325] (" ")
                    newLine [15325-15326]
                    fixedWidth [15326-15329]
                      operator [15326-15328] (": ")
                      text [15328-15329] ("4")
                    newLine [15329-15330]
                    newLine [15330-15331]
                    keyword [15331-15348]
                      text [15331-15348] ("#+CLOSE_{SPOILER}")
                    newLine [15348-15349]
                  section [15171-15351]
                    indent [15171-15173] ("  ")
                    text [15173-15197] ("(while (< my-counter 12)")
                    newLine [15197-15198]
                    indent [15198-15202] ("    ")
                    text [15202-15222] ("(if (> my-counter 3)")
                    newLine [15222-15223]
                    indent [15223-15231] ("        ")
                    text [15231-15256] ("(return-from my-iterator)")
                    newLine [15256-15257]
                    indent [15257-15263] ("      ")
                    text [15263-15264] (")")
                    newLine [15264-15265]
                    indent [15265-15269] ("    ")
                    text [15269-15304] ("(setq my-counter (+ my-counter 1)))")
                    newLine [15304-15305]
                    indent [15305-15307] ("  ")
                    text [15307-15308] (")")
                    newLine [15308-15309]
                    newLine [15309-15310]
                    text [15310-15323] ("(my-iterator)")
                    newLine [15323-15324]
                    newLine [15324-15325]
                    text [15325-15350] ("(message \\"%s\\" my-counter)")
                    newLine [15350-15351]
            headline [15664-20337]
                :level 2:
              title [15664-15718]
                operator [15664-15667] ("** ")
                text [15667-15689] ("Взаимодействие с emacs")
                newLine [15689-15690]
                keyword [15690-15715]
                  text [15690-15715] ("#+START_{SPOILER} Детали ")
                text [15715-15716] (">")
                newLine [15716-15717]
                newLine [15717-15718]
              section [15690-20309]
                headline [15690-15773]
                    :level 3:
                  title [15690-15773]
                    operator [15690-15694] ("*** ")
                    text [15694-15710] ("Вставка в текста")
                    newLine [15710-15711]
                    srcBlock [15711-15772]
                      blockHeader [15711-15733]
                        keyword [15711-15733]
                          text [15711-15722] ("#+begin_src")
                          text [15722-15733] (" emacs-lisp")
                      newLine [15733-15734]
                      blockBody [15734-15762]
                        text [15734-15762] ("(insert \\"Hello\\" \\" \\" \\"World\\")")
                      newLine [15762-15763]
                      blockFooter [15763-15772]
                        keyword [15763-15772]
                          text [15763-15772] ("#+end_src")
                    newLine [15772-15773]
                  section [15711-15711]
                headline [15773-16356]
                    :level 3:
                  title [15773-15794]
                    operator [15773-15777] ("*** ")
                    text [15777-15793] ("Работа с буфером")
                    newLine [15793-15794]
                  section [15794-16356]
                    headline [15794-15969]
                        :level 4:
                      title [15794-15867]
                        operator [15794-15799] ("**** ")
                        text [15799-15833] ("Программное создание нового буфера")
                        newLine [15833-15834]
                        srcBlock [15834-15866]
                          blockHeader [15834-15856]
                            keyword [15834-15856]
                              text [15834-15845] ("#+begin_src")
                              text [15845-15856] (" emacs-lisp")
                          newLine [15856-15857]
                          blockFooter [15857-15866]
                            keyword [15857-15866]
                              text [15857-15866] ("#+end_src")
                        newLine [15866-15867]
                      section [15834-15936]
                        indent [15834-15836] ("  ")
                        text [15836-15868] ("(switch-to-buffer-other-window \\"")
                        bold [15868-15885]
                          operator [15868-15869] ("*")
                          text [15869-15884] ("my-first-buffer")
                          operator [15884-15885] ("*")
                        text [15885-15887] ("\\")")
                        newLine [15887-15888]
                        indent [15888-15890] ("  ")
                        text [15890-15935] ("(insert \\"Congratulations! I'am a new buffer\\")")
                        newLine [15935-15936]
                    headline [15969-16037]
                        :level 4:
                      title [15969-16037]
                        operator [15969-15974] ("**** ")
                        text [15974-15988] ("Очистка буфера")
                        newLine [15988-15989]
                        srcBlock [15989-16036]
                          blockHeader [15989-16011]
                            keyword [15989-16011]
                              text [15989-16000] ("#+begin_src")
                              text [16000-16011] (" emacs-lisp")
                          newLine [16011-16012]
                          blockBody [16012-16026]
                            text [16012-16026] ("(erase-buffer)")
                          newLine [16026-16027]
                          blockFooter [16027-16036]
                            keyword [16027-16036]
                              text [16027-16036] ("#+end_src")
                        newLine [16036-16037]
                      section [15989-15989]
                    headline [16037-16356]
                        :level 4:
                      title [16037-16105]
                        operator [16037-16042] ("**** ")
                        text [16042-16060] ("Интерактивный ввод")
                        newLine [16060-16061]
                        srcBlock [16061-16093]
                          blockHeader [16061-16083]
                            keyword [16061-16083]
                              text [16061-16072] ("#+begin_src")
                              text [16072-16083] (" emacs-lisp")
                          newLine [16083-16084]
                          blockFooter [16084-16093]
                            keyword [16084-16093]
                              text [16084-16093] ("#+end_src")
                        newLine [16093-16094]
                        keyword [16094-16104]
                          text [16094-16104] ("#+RESULTS:")
                        newLine [16104-16105]
                      section [16061-16312]
                        indent [16061-16063] ("  ")
                        text [16063-16108] (";; (read-from-minibuffer \\"Enter your name: \\")")
                        newLine [16108-16109]
                        indent [16109-16111] ("  ")
                        text [16111-16172] ("(let ((your-name (read-from-minibuffer \\"Enter your name: \\")))")
                        newLine [16172-16173]
                        indent [16173-16179] ("      ")
                        text [16179-16232] ("(switch-to-buffer-other-window \\"*Your personal info\\")")
                        newLine [16232-16233]
                        indent [16233-16235] ("  ")
                        text [16235-16249] ("(erase-buffer)")
                        newLine [16249-16250]
                        indent [16250-16252] ("  ")
                        text [16252-16291] ("(insert (format \\"Hello %s!\\" your-name))")
                        newLine [16291-16292]
                        indent [16292-16294] ("  ")
                        text [16294-16311] ("(other-window 1))")
                        newLine [16311-16312]
                headline [16356-17756]
                    :level 3:
                  title [16356-17364]
                    operator [16356-16360] ("*** ")
                    text [16360-16376] ("Replace в буфере")
                    newLine [16376-16377]
                    srcBlock [16377-16409]
                      blockHeader [16377-16399]
                        keyword [16377-16399]
                          text [16377-16388] ("#+begin_src")
                          text [16388-16399] (" emacs-lisp")
                      newLine [16399-16400]
                      blockFooter [16400-16409]
                        keyword [16400-16409]
                          text [16400-16409] ("#+end_src")
                    newLine [16409-16410]
                    bold [16410-16421]
                      operator [16410-16411] ("*")
                      text [16411-16420] ("goto-char")
                      operator [16420-16421] ("*")
                    text [16421-16422] (" ")
                    list [16422-16470]
                        :unordered:
                        :level 0:
                      listItem [16422-16454]
                        title [16422-16454]
                          operator [16422-16424] ("- ")
                          text [16424-16453] ("переход к конкретному символу")
                          newLine [16453-16454]
                      listItem [16454-16470]
                        title [16454-16470]
                          operator [16454-16456] ("- ")
                          text [16456-16469] ("начало буфера")
                          newLine [16469-16470]
                    bold [16470-16481]
                      operator [16470-16471] ("*")
                      text [16471-16480] ("point-min")
                      operator [16480-16481] ("*")
                    text [16481-16482] (" ")
                    headline [16482-16961]
                        :level 3:
                      title [16482-16516]
                        operator [16482-16486] ("*** ")
                        text [16486-16515] ("Добавление свойств для текста")
                        newLine [16515-16516]
                      section [16516-16961]
                        indent [16516-16518] ("  ")
                        text [16518-16538] (";; (detect-bad-boys)")
                        newLine [16538-16539]
                        indent [16539-16541] ("  ")
                        newLine [16541-16542]
                        indent [16542-16544] ("  ")
                        newLine [16544-16545]
                        indent [16545-16547] ("  ")
                        text [16547-16573] ("(defun boldify-bad-boys ()")
                        newLine [16573-16574]
                        indent [16574-16578] ("    ")
                        text [16578-16610] ("(switch-to-buffer-other-window \\"")
                        bold [16610-16623]
                          operator [16610-16611] ("*")
                          text [16611-16622] ("lisp lesson")
                          operator [16622-16623] ("*")
                        text [16623-16625] ("\\")")
                        newLine [16625-16626]
                        indent [16626-16630] ("    ")
                        text [16630-16653] ("(goto-char (point-min))")
                        newLine [16653-16654]
                        indent [16654-16658] ("    ")
                        text [16658-16695] ("(while (re-search-forward \\"Awful boy ")
                        keyword [16695-16698]
                          text [16695-16698] ("\\\\(.")
                        text [16698-16699] ("+")
                        keyword [16699-16709]
                          text [16699-16702] ("\\\\)\\"")
                          text [16702-16709] (" nil t)")
                        newLine [16709-16710]
                        indent [16710-16716] ("      ")
                        text [16716-16763] ("(message (format \\"Its %s\\" (match-beginning 1)))")
                        newLine [16763-16764]
                        indent [16764-16770] ("      ")
                        text [16770-16810] ("(add-text-properties (match-beginning 1)")
                        newLine [16810-16811]
                        indent [16811-16838] ("                           ")
                        text [16838-16851] ("(match-end 1)")
                        newLine [16851-16852]
                        indent [16852-16879] ("                           ")
                        text [16879-16906] ("(list 'face 'bold-italic)))")
                        newLine [16906-16907]
                        indent [16907-16911] ("    ")
                        text [16911-16930] (";; (other-window 1)")
                        newLine [16930-16931]
                        indent [16931-16935] ("    ")
                        text [16935-16936] (")")
                        newLine [16936-16937]
                        indent [16937-16939] ("  ")
                        newLine [16939-16940]
                        indent [16940-16942] ("  ")
                        text [16942-16960] ("(boldify-bad-boys)")
                        newLine [16960-16961]
                    italic [16961-17013]
                      operator [16961-16962] ("/")
                      text [16962-17012] ("Перед этим необходимо запустить предыдущую функцию")
                      operator [17012-17013] ("/")
                    newLine [17013-17014]
                    newLine [17014-17015]
                    srcBlock [17015-17047]
                      blockHeader [17015-17037]
                        keyword [17015-17037]
                          text [17015-17026] ("#+begin_src")
                          text [17026-17037] (" emacs-lisp")
                      newLine [17037-17038]
                      blockFooter [17038-17047]
                        keyword [17038-17047]
                          text [17038-17047] ("#+end_src")
                    newLine [17047-17048]
                    keyword [17048-17059]
                      text [17048-17058] ("#+RESULTS:")
                      text [17058-17059] (" ")
                    newLine [17059-17060]
                    text [17060-17084] ("Про сумасшедшие регекспы")
                    newLine [17084-17085]
                    newLine [17085-17086]
                    quoteBlock [17086-17363]
                      blockHeader [17086-17099]
                        keyword [17086-17099]
                          text [17086-17099] ("#+begin_quote")
                      newLine [17099-17100]
                      blockBody [17100-17351]
                        text [17100-17138] (";; The regular expression is \\"Bonjour ")
                        keyword [17138-17141]
                          text [17138-17141] ("\\\\(.")
                        text [17141-17142] ("+")
                        keyword [17142-17160]
                          text [17142-17146] ("\\\\)!\\"")
                          text [17146-17160] (" and it reads:")
                        newLine [17160-17161]
                        text [17161-17190] (";; the string \\"Bonjour \\", and")
                        newLine [17190-17191]
                        text [17191-17229] (";; a group of           | this is the ")
                        keyword [17229-17236]
                          text [17229-17231] ("\\\\(")
                          text [17231-17236] (" ... ")
                        keyword [17236-17248]
                          text [17236-17238] ("\\\\)")
                          text [17238-17248] (" construct")
                        newLine [17248-17249]
                        text [17249-17288] (";;   any character      | this is the .")
                        newLine [17288-17289]
                        text [17289-17328] (";;   possibly repeated  | this is the +")
                        newLine [17328-17329]
                        text [17329-17351] (";; and the \\"!\\" string.")
                      newLine [17351-17352]
                      blockFooter [17352-17363]
                        keyword [17352-17363]
                          text [17352-17363] ("#+end_quote")
                    newLine [17363-17364]
                  section [16377-16769]
                    indent [16377-16379] ("  ")
                    text [16379-16404] ("(defun detect-bad-boys ()")
                    newLine [16404-16405]
                    indent [16405-16409] ("    ")
                    text [16409-16457] ("(setq lesson-list '(\\"Buzova\\" \\"Volodin\\" \\"Pupin\\"))")
                    newLine [16457-16458]
                    indent [16458-16460] ("  ")
                    newLine [16460-16461]
                    indent [16461-16465] ("    ")
                    text [16465-16490] ("(defun mark-as-bad (name)")
                    newLine [16490-16491]
                    indent [16491-16497] ("      ")
                    text [16497-16525] ("(insert (format \\"Bad boy %s ")
                    newLine [16525-16526]
                    text [16526-16535] ("\\" name)))")
                    newLine [16535-16536]
                    indent [16536-16538] ("  ")
                    newLine [16538-16539]
                    indent [16539-16543] ("    ")
                    text [16543-16575] ("(switch-to-buffer-other-window \\"")
                    bold [16575-16588]
                      operator [16575-16576] ("*")
                      text [16576-16587] ("lisp lesson")
                      operator [16587-16588] ("*")
                    text [16588-16590] ("\\")")
                    newLine [16590-16591]
                    indent [16591-16595] ("    ")
                    text [16595-16628] ("(mapcar 'mark-as-bad lesson-list)")
                    newLine [16628-16629]
                    indent [16629-16633] ("    ")
                    text [16633-16656] ("(goto-char (point-min))")
                    newLine [16656-16657]
                    indent [16657-16661] ("    ")
                    text [16661-16690] ("(while (search-forward \\"Bad\\")")
                    newLine [16690-16691]
                    indent [16691-16697] ("      ")
                    text [16697-16721] ("(replace-match \\"Awful\\"))")
                    newLine [16721-16722]
                    indent [16722-16726] ("    ")
                    text [16726-16742] ("(other-window 1)")
                    newLine [16742-16743]
                    indent [16743-16747] ("    ")
                    text [16747-16748] (")")
                    newLine [16748-16749]
                    indent [16749-16751] ("  ")
                    text [16751-16768] ("(detect-bad-boys)")
                    newLine [16768-16769]
                headline [17756-18411]
                    :level 3:
                  title [17756-18090]
                    operator [17756-17760] ("*** ")
                    text [17760-17777] ("Создание кнопочки")
                    newLine [17777-17778]
                    text [17778-17840] ("Даннный метод создает кнопку над текстом с позиции от 1 до 10.")
                    newLine [17840-17841]
                    newLine [17841-17842]
                    srcBlock [17842-17911]
                      blockHeader [17842-17864]
                        keyword [17842-17864]
                          text [17842-17853] ("#+begin_src")
                          text [17853-17864] (" emacs-lisp")
                      newLine [17864-17865]
                      blockBody [17865-17901]
                        text [17865-17901] ("(defun butest-varon-pressed (button)")
                      newLine [17901-17902]
                      blockFooter [17902-17911]
                        keyword [17902-17911]
                          text [17902-17911] ("#+end_src")
                    newLine [17911-17912]
                    text [17912-17972] ("Данная функция вставляет кнопку под текущей позицей каретки.")
                    newLine [17972-17973]
                    newLine [17973-17974]
                    srcBlock [17974-18038]
                      blockHeader [17974-17996]
                        keyword [17974-17996]
                          text [17974-17985] ("#+begin_src")
                          text [17985-17996] (" emacs-lisp")
                      newLine [17996-17997]
                      blockBody [17997-18028]
                        text [17997-18028] ("(insert-butest-varon \\"Press me\\"")
                      newLine [18028-18029]
                      blockFooter [18029-18038]
                        keyword [18029-18038]
                          text [18029-18038] ("#+end_src")
                    newLine [18038-18039]
                    keyword [18039-18050]
                      text [18039-18049] ("#+RESULTS:")
                      text [18049-18050] (" ")
                    newLine [18050-18051]
                    fixedWidth [18051-18089]
                      operator [18051-18053] (": ")
                      text [18053-18089] ("#<overlay from 1 to 10 in elisp.org>")
                    newLine [18089-18090]
                  section [17778-18099]
                    indent [17778-17780] ("  ")
                    text [17780-17823] ("(message (format \\"Butest-varon pressed!\\")))")
                    newLine [17823-17824]
                    newLine [17824-17825]
                    text [17825-17865] ("(define-butest-varon-type 'custom-button")
                    newLine [17865-17866]
                    indent [17866-17868] ("  ")
                    text [17868-17897] ("'action 'butest-varon-pressed")
                    newLine [17897-17898]
                    indent [17898-17900] ("  ")
                    text [17900-17914] ("'follow-link t")
                    newLine [17914-17915]
                    indent [17915-17917] ("  ")
                    text [17917-17948] ("'help-echo \\"Click Butest-varon\\"")
                    newLine [17948-17949]
                    indent [17949-17951] ("  ")
                    text [17951-17969] ("'help-args \\"test\\")")
                    newLine [17969-17970]
                    newLine [17970-17971]
                    text [17971-17995] ("(make-butest-varon 1 10 ")
                    blockProperty [17995-18016]
                      text [17995-18000] (":type")
                      text [18000-18016] (" 'custom-button)")
                    newLine [18016-18017]
                    indent [18017-18032] ("               ")
                    text [18032-18098] ("'action (lambda (_arg) (print \\"You are press the butest-varon!\\")))")
                    newLine [18098-18099]
                headline [18411-18523]
                    :level 3:
                  title [18411-18523]
                    operator [18411-18415] ("*** ")
                    text [18415-18435] ("Чтение из completion")
                    newLine [18435-18436]
                    srcBlock [18436-18522]
                      blockHeader [18436-18458]
                        keyword [18436-18458]
                          text [18436-18447] ("#+begin_src")
                          text [18447-18458] (" emacs-lisp")
                      newLine [18458-18459]
                      blockBody [18459-18512]
                        text [18459-18512] ("(completing-read \\"Choose one: \\" '(\\"foo\\" \\"bar\\" \\"baz\\"))")
                      newLine [18512-18513]
                      blockFooter [18513-18522]
                        keyword [18513-18522]
                          text [18513-18522] ("#+end_src")
                    newLine [18522-18523]
                  section [18436-18436]
                headline [18523-18678]
                    :level 3:
                  title [18523-18678]
                    operator [18523-18527] ("*** ")
                    text [18527-18548] ("Пользовательский ввод")
                    newLine [18548-18549]
                    srcBlock [18549-18638]
                      blockHeader [18549-18571]
                        keyword [18549-18571]
                          text [18549-18560] ("#+begin_src")
                          text [18560-18571] (" emacs-lisp")
                      newLine [18571-18572]
                      blockBody [18572-18628]
                        text [18572-18628] ("(message \\"U say: %s\\" (read-string \\"Say me something: \\"))")
                      newLine [18628-18629]
                      blockFooter [18629-18638]
                        keyword [18629-18638]
                          text [18629-18638] ("#+end_src")
                    newLine [18638-18639]
                    keyword [18639-18650]
                      text [18639-18649] ("#+RESULTS:")
                      text [18649-18650] (" ")
                    newLine [18650-18651]
                    fixedWidth [18651-18677]
                      operator [18651-18653] (": ")
                      text [18653-18677] ("U say: Ну че тут скажешь")
                    newLine [18677-18678]
                  section [18549-18549]
                headline [18678-18864]
                    :level 3:
                  title [18678-18710]
                    operator [18678-18682] ("*** ")
                    text [18682-18709] ("Работа с выделенным текстом")
                    newLine [18709-18710]
                  section [18710-18864]
                    headline [18710-18761]
                        :level 4:
                      title [18710-18761]
                        operator [18710-18715] ("**** ")
                        text [18715-18743] ("Проверка что что-то выделено")
                        newLine [18743-18744]
                        verbatim [18744-18760]
                          operator [18744-18745] ("=")
                          text [18745-18759] ("(use-region-p)")
                          operator [18759-18760] ("=")
                        newLine [18760-18761]
                      section [18744-18744]
                    headline [18761-18864]
                        :level 4:
                      title [18761-18864]
                        operator [18761-18766] ("**** ")
                        text [18766-18791] ("Получить выделенный текст")
                        newLine [18791-18792]
                        srcBlock [18792-18863]
                          blockHeader [18792-18814]
                            keyword [18792-18814]
                              text [18792-18803] ("#+begin_src")
                              text [18803-18814] (" emacs-lisp")
                          newLine [18814-18815]
                          blockBody [18815-18853]
                            text [18815-18853] ("(regionp (buffer-substring start end))")
                          newLine [18853-18854]
                          blockFooter [18854-18863]
                            keyword [18854-18863]
                              text [18854-18863] ("#+end_src")
                        newLine [18863-18864]
                      section [18792-18792]
                headline [18864-19083]
                    :level 3:
                  title [18864-19083]
                    operator [18864-18868] ("*** ")
                    text [18868-18909] ("Конвертация символа в строку (ну и назад)")
                    newLine [18909-18910]
                    srcBlock [18910-19039]
                      blockHeader [18910-18932]
                        keyword [18910-18932]
                          text [18910-18921] ("#+begin_src")
                          text [18921-18932] (" emacs-lisp")
                      newLine [18932-18933]
                      blockBody [18933-19029]
                        text [18933-19029] ("(symbol-name 'something) ;; Символ в строку\\n(intern (symbol-name 'something)) ;; Строка в символ")
                      newLine [19029-19030]
                      blockFooter [19030-19039]
                        keyword [19030-19039]
                          text [19030-19039] ("#+end_src")
                    newLine [19039-19040]
                    keyword [19040-19051]
                      text [19040-19050] ("#+RESULTS:")
                      text [19050-19051] (" ")
                    newLine [19051-19052]
                    fixedWidth [19052-19063]
                      operator [19052-19054] (": ")
                      text [19054-19063] ("something")
                    newLine [19063-19064]
                    newLine [19064-19065]
                    keyword [19065-19082]
                      text [19065-19082] ("#+CLOSE_{SPOILER}")
                    newLine [19082-19083]
                  section [18910-18910]
                headline [19083-20164]
                    :level 3:
                  title [19083-19277]
                    operator [19083-19087] ("*** ")
                    text [19087-19094] ("Overlay")
                    newLine [19094-19095]
                    text [19095-19275] ("Overlay это очень крутая тема. Он позволяет рендерить текст который не изменяет контент реального буфера. Это может быть полезно для показа подсказок, дебага, расчитанных значений.")
                    newLine [19275-19276]
                    newLine [19276-19277]
                  section [19095-19982]
                    headline [19095-19244]
                        :level 4:
                      title [19095-19244]
                        operator [19095-19100] ("**** ")
                        text [19100-19131] ("Создание оверлея в конце строки")
                        newLine [19131-19132]
                        srcBlock [19132-19243]
                          blockHeader [19132-19154]
                            keyword [19132-19154]
                              text [19132-19143] ("#+begin_src")
                              text [19143-19154] (" emacs-lisp")
                          newLine [19154-19155]
                          blockBody [19155-19233]
                            text [19155-19233] ("(setq my-first-overlay (make-overlay (line-end-position) (line-end-position)))")
                          newLine [19233-19234]
                          blockFooter [19234-19243]
                            keyword [19234-19243]
                              text [19234-19243] ("#+end_src")
                        newLine [19243-19244]
                      section [19132-19132]
                    headline [19244-19539]
                        :level 4:
                      title [19244-19539]
                        operator [19244-19249] ("**** ")
                        text [19249-19281] ("Курсор заходит за предел оверлея")
                        newLine [19281-19282]
                        text [19282-19412] ("В моем случае курсор выходил за предел оверлея. Решается весьма просто: вставляемый в оверлей текст необходимо наделить свойством ")
                        verbatim [19412-19423]
                          operator [19412-19413] ("=")
                          text [19413-19422] ("'cursor t")
                          operator [19422-19423] ("=")
                        newLine [19423-19424]
                        newLine [19424-19425]
                        srcBlock [19425-19538]
                          blockHeader [19425-19447]
                            keyword [19425-19447]
                              text [19425-19436] ("#+begin_src")
                              text [19436-19447] (" emacs-lisp")
                          newLine [19447-19448]
                          blockBody [19448-19528]
                            text [19448-19528] ("(setq my-popup-message (propertize popup-message 'face 'blamer--face 'cursor t))")
                          newLine [19528-19529]
                          blockFooter [19529-19538]
                            keyword [19529-19538]
                              text [19529-19538] ("#+end_src")
                        newLine [19538-19539]
                      section [19282-19282]
                    headline [19539-19849]
                        :level 4:
                      title [19539-19603]
                        operator [19539-19544] ("**** ")
                        text [19544-19569] ("Изменение свойств overlay")
                        newLine [19569-19570]
                        srcBlock [19570-19602]
                          blockHeader [19570-19592]
                            keyword [19570-19592]
                              text [19570-19581] ("#+begin_src")
                              text [19581-19592] (" emacs-lisp")
                          newLine [19592-19593]
                          blockFooter [19593-19602]
                            keyword [19593-19602]
                              text [19593-19602] ("#+end_src")
                        newLine [19602-19603]
                      section [19570-19816]
                        indent [19570-19574] ("    ")
                        text [19574-19642] ("(overlay-put blamer--current-overlay 'after-string my-popup-message)")
                        newLine [19642-19643]
                        indent [19643-19647] ("    ")
                        text [19647-19698] ("(overlay-put blamer--current-overlay 'intangible t)")
                        newLine [19698-19699]
                        indent [19699-19703] ("    ")
                        text [19703-19752] ("(overlay-put blamer--current-overlay 'face 'bold)")
                        newLine [19752-19753]
                        indent [19753-19757] ("    ")
                        text [19757-19815] ("(overlay-put blamer--current-overlay 'cursor-intangible t)")
                        newLine [19815-19816]
                    headline [19849-19982]
                        :level 4:
                      title [19849-19939]
                        operator [19849-19854] ("**** ")
                        text [19854-19884] ("Удаление существующего оверлея")
                        newLine [19884-19885]
                        srcBlock [19885-19938]
                          blockHeader [19885-19907]
                            keyword [19885-19907]
                              text [19885-19896] ("#+begin_src")
                              text [19896-19907] (" emacs-lisp")
                          newLine [19907-19908]
                          blockBody [19908-19928]
                            text [19908-19928] ("(if my-first-overlay")
                          newLine [19928-19929]
                          blockFooter [19929-19938]
                            keyword [19929-19938]
                              text [19929-19938] ("#+end_src")
                        newLine [19938-19939]
                      section [19885-19928]
                        indent [19885-19893] ("        ")
                        text [19893-19927] ("(delete-overlay my-first-overlay))")
                        newLine [19927-19928]
                headline [20164-20309]
                    :level 3:
                  title [20164-20309]
                    operator [20164-20168] ("*** ")
                    text [20168-20195] ("Создание своего minor-mode ")
                    tagList [20195-20200]
                      operator [20195-20196] (":")
                      text [20196-20199] ("WIP")
                      operator [20199-20200] (":")
                    newLine [20200-20201]
                    link [20201-20308]
                      operator [20201-20202] ("[")
                      linkUrl [20202-20293]
                        operator [20202-20203] ("[")
                        text [20203-20292] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Minor-Modes.html")
                        operator [20292-20293] ("]")
                      linkName [20293-20307]
                        operator [20293-20294] ("[")
                        text [20294-20306] ("Документация")
                        operator [20306-20307] ("]")
                      operator [20307-20308] ("]")
                    newLine [20308-20309]
                  section [20201-20201]
            headline [20337-20726]
                :level 2:
              title [20337-20726]
                operator [20337-20340] ("** ")
                text [20340-20355] ("Работа с датами")
                newLine [20355-20356]
                link [20356-20481]
                  operator [20356-20357] ("[")
                  linkUrl [20357-20453]
                    operator [20357-20358] ("[")
                    text [20358-20452] ("htest-varps://stackoverflow.com/questions/4242012/how-do-i-add-dates-in-emacs-using-emacs-lisp")
                    operator [20452-20453] ("]")
                  linkName [20453-20480]
                    operator [20453-20454] ("[")
                    text [20454-20479] ("Ага, любимый стаковерфлоу")
                    operator [20479-20480] ("]")
                  operator [20480-20481] ("]")
                newLine [20481-20482]
                newLine [20482-20483]
                srcBlock [20483-20573]
                  blockHeader [20483-20505]
                    keyword [20483-20505]
                      text [20483-20494] ("#+begin_src")
                      text [20494-20505] (" emacs-lisp")
                  newLine [20505-20506]
                  blockBody [20506-20563]
                    text [20506-20563] ("(setq t3 (time-subtract (current-time) (days-to-time 2)))")
                  newLine [20563-20564]
                  blockFooter [20564-20573]
                    keyword [20564-20573]
                      text [20564-20573] ("#+end_src")
                newLine [20573-20574]
                table [20574-20604]
                  tableRow [20574-20603]
                    operator [20574-20575] ("|")
                    tableCell [20575-20582]
                      text [20575-20582] (" 24939 ")
                    operator [20582-20583] ("|")
                    tableCell [20583-20589]
                      text [20583-20589] (" 1255 ")
                    operator [20589-20590] ("|")
                    tableCell [20590-20598]
                      text [20590-20598] (" 721279 ")
                    operator [20598-20599] ("|")
                    tableCell [20599-20602]
                      text [20599-20602] (" 0 ")
                    operator [20602-20603] ("|")
                  newLine [20603-20604]
                srcBlock [20604-20692]
                  blockHeader [20604-20626]
                    keyword [20604-20626]
                      text [20604-20615] ("#+begin_src")
                      text [20615-20626] (" emacs-lisp")
                  newLine [20626-20627]
                  blockBody [20627-20682]
                    text [20627-20682] ("\\n(message \\"%s\\" (/ (float-time (time-since t3)) (* 60)))")
                  newLine [20682-20683]
                  blockFooter [20683-20692]
                    keyword [20683-20692]
                      text [20683-20692] ("#+end_src")
                newLine [20692-20693]
                keyword [20693-20704]
                  text [20693-20703] ("#+RESULTS:")
                  text [20703-20704] (" ")
                newLine [20704-20705]
                fixedWidth [20705-20725]
                  operator [20705-20707] (": ")
                  text [20707-20725] ("2940.0710639333333")
                newLine [20725-20726]
              section [20356-20356]
            headline [20726-21813]
                :level 2:
              title [20726-20736]
                operator [20726-20729] ("** ")
                text [20729-20735] ("Regexp")
                newLine [20735-20736]
              section [20736-21813]
                headline [20736-21406]
                    :level 3:
                  title [20736-21396]
                    operator [20736-20740] ("*** ")
                    text [20740-20747] ("Примеры")
                    newLine [20747-20748]
                    text [20748-20793] ("Просто кучка примеров из разработанного мной ")
                    link [20793-20849]
                      operator [20793-20794] ("[")
                      linkUrl [20794-20840]
                        operator [20794-20795] ("[")
                        text [20795-20839] ("htest-varps://github.com/Artawower/turbo-log")
                        operator [20839-20840] ("]")
                      linkName [20840-20848]
                        operator [20840-20841] ("[")
                        text [20841-20847] ("пакета")
                        operator [20847-20848] ("]")
                      operator [20848-20849] ("]")
                    text [20849-21025] (". Регекспы весьма похожи на то что представлено в других языках. Сложно лишь работать с интерполяцией строк (неочевидна работа с большим количеством слешей в исполняемом коде.)")
                    newLine [21025-21026]
                    newLine [21026-21027]
                    srcBlock [21027-21108]
                      blockHeader [21027-21049]
                        keyword [21027-21049]
                          text [21027-21038] ("#+begin_src")
                          text [21038-21049] (" emacs-lisp")
                      newLine [21049-21050]
                      blockBody [21050-21098]
                        text [21050-21098] ("(string-match \\"^\\\\(\\\\)*\\\\(return\\\\)\\" \\"  return {\\n}\\")")
                      newLine [21098-21099]
                      blockFooter [21099-21108]
                        keyword [21099-21108]
                          text [21099-21108] ("#+end_src")
                    newLine [21108-21109]
                    keyword [21109-21120]
                      text [21109-21119] ("#+RESULTS:")
                      text [21119-21120] (" ")
                    newLine [21120-21121]
                    fixedWidth [21121-21124]
                      operator [21121-21123] (": ")
                      text [21123-21124] ("0")
                    newLine [21124-21125]
                    newLine [21125-21126]
                    newLine [21126-21127]
                    srcBlock [21127-21256]
                      blockHeader [21127-21149]
                        keyword [21127-21149]
                          text [21127-21138] ("#+begin_src")
                          text [21138-21149] (" emacs-lisp")
                      newLine [21149-21150]
                      blockBody [21150-21246]
                        text [21150-21246] ("(replace-regexp-in-string \\"[[:blank:]*=[[:blank:]*.+\\" \\"\\" \\"    this.myVariable = somethingElse;\\")")
                      newLine [21246-21247]
                      blockFooter [21247-21256]
                        keyword [21247-21256]
                          text [21247-21256] ("#+end_src")
                    newLine [21256-21257]
                    srcBlock [21257-21364]
                      blockHeader [21257-21279]
                        keyword [21257-21279]
                          text [21257-21268] ("#+begin_src")
                          text [21268-21279] (" emacs-lisp")
                      newLine [21279-21280]
                      blockBody [21280-21354]
                        text [21280-21354] ("(replace-regexp-in-string \\"\\\\(const\\\\|let\\\\|public\\\\|protected\\\\|private\\\\|var\\\\)")
                      newLine [21354-21355]
                      blockFooter [21355-21364]
                        keyword [21355-21364]
                          text [21355-21364] ("#+end_src")
                    newLine [21364-21365]
                    keyword [21365-21376]
                      text [21365-21375] ("#+RESULTS:")
                      text [21375-21376] (" ")
                    newLine [21376-21377]
                    fixedWidth [21377-21395]
                      operator [21377-21379] (": ")
                      text [21379-21395] ("iable = userName")
                    newLine [21395-21396]
                  section [20748-20758]
                    indent [20748-20750] ("  ")
                    text [20750-20757] ("name: 2")
                    newLine [20757-20758]
                headline [21406-21813]
                    :level 3:
                  title [21406-21699]
                    operator [21406-21410] ("*** ")
                    text [21410-21431] ("Regexp с группировкой")
                    newLine [21431-21432]
                    srcBlock [21432-21499]
                      blockHeader [21432-21454]
                        keyword [21432-21454]
                          text [21432-21443] ("#+begin_src")
                          text [21443-21454] (" emacs-lisp")
                      newLine [21454-21455]
                      blockBody [21455-21489]
                        text [21455-21489] ("(concat \\"^(?\\\\(?1:[^s]\\\\) [^s]\\n]+\\\\)\\"")
                      newLine [21489-21490]
                      blockFooter [21490-21499]
                        keyword [21490-21499]
                          text [21490-21499] ("#+end_src")
                    newLine [21499-21500]
                    srcBlock [21500-21680]
                      blockHeader [21500-21522]
                        keyword [21500-21522]
                          text [21500-21511] ("#+begin_src")
                          text [21511-21522] (" emacs-lisp")
                      newLine [21522-21523]
                      blockBody [21523-21670]
                        text [21523-21670] ("(setq test-string \\"feature/VW-221\\")\\n(string-match \\"\\\\(?1:[A-Za-z0-9]+/\\\\)\\\\(?2:VW-[0-9]+\\\\)\\" test-string)\\n(message \\"res \\" (match-string 1 test-string))")
                      newLine [21670-21671]
                      blockFooter [21671-21680]
                        keyword [21671-21680]
                          text [21671-21680] ("#+end_src")
                    newLine [21680-21681]
                    keyword [21681-21692]
                      text [21681-21691] ("#+RESULTS:")
                      text [21691-21692] (" ")
                    newLine [21692-21693]
                    fixedWidth [21693-21698]
                      operator [21693-21695] (": ")
                      text [21695-21698] ("res")
                    newLine [21698-21699]
                  section [21432-21546]
                    indent [21432-21442] ("          ")
                    text [21442-21484] ("\\"s\\\\(?3:[0-9]-undefined\\\\{2\\\\}-undefined\\\\{2\\\\}")
                    keyword [21484-21487]
                      text [21484-21487] ("\\\\)\\"")
                    newLine [21487-21488]
                    indent [21488-21498] ("          ")
                    text [21498-21516] ("\\"s\\\\(?4:[0-9]\\\\{2\\\\}:")
                    text [21516-21541] ("undefined\\\\{2\\\\}:undefined\\\\{2\\\\}")
                      operator [21516-21517] ("[")
                      text [21517-21520] ("0-9")
                      operator [21520-21521] ("]")
                    keyword [21541-21545]
                      text [21541-21545] ("\\\\)\\")")
                    newLine [21545-21546]
            headline [21813-21913]
                :level 2:
              title [21813-21913]
                operator [21813-21816] ("** ")
                text [21816-21832] ("Стандартные хуки")
                newLine [21832-21833]
                link [21833-21912]
                  operator [21833-21834] ("[")
                  linkUrl [21834-21891]
                    operator [21834-21835] ("[")
                    text [21835-21890] ("htest-varps://runebook.dev/ru/docs/elisp/standard-hooks")
                    operator [21890-21891] ("]")
                  linkName [21891-21911]
                    operator [21891-21892] ("[")
                    text [21892-21910] ("Просто смотри сюда")
                    operator [21910-21911] ("]")
                  operator [21911-21912] ("]")
                newLine [21912-21913]
              section [21833-21833]
            headline [21913-23145]
                :level 2:
              title [21913-21929]
                operator [21913-21916] ("** ")
                text [21916-21928] ("Custom modes")
                newLine [21928-21929]
              section [21929-23145]
                headline [21929-23145]
                    :level 3:
                  title [21929-22883]
                    operator [21929-21933] ("*** ")
                    text [21933-21943] ("Minor mode")
                    newLine [21943-21944]
                    text [21944-22051] ("Для того чтобы сделать свой minor mode достаточно его объявить и описать логику включения/выключений режима")
                    newLine [22051-22052]
                    newLine [22052-22053]
                    srcBlock [22053-22136]
                      blockHeader [22053-22075]
                        keyword [22053-22075]
                          text [22053-22064] ("#+begin_src")
                          text [22064-22075] (" emacs-lisp")
                      newLine [22075-22076]
                      blockBody [22076-22126]
                        text [22076-22126] (";;;###autoload\\n(define-minor-mode wakatime-ui-mode")
                      newLine [22126-22127]
                      blockFooter [22127-22136]
                        keyword [22127-22136]
                          text [22127-22136] ("#+end_src")
                    newLine [22136-22137]
                    text [22137-22141] ("Где:")
                    newLine [22141-22142]
                    newLine [22142-22143]
                    verbatim [22143-22155]
                      operator [22143-22144] ("=")
                      text [22144-22154] ("init-value")
                      operator [22154-22155] ("=")
                    text [22155-22156] (" ")
                    list [22156-22293]
                        :unordered:
                        :level 0:
                      listItem [22156-22180]
                        title [22156-22180]
                          operator [22156-22158] ("- ")
                          text [22158-22179] ("значение по умолчанию")
                          newLine [22179-22180]
                      listItem [22180-22236]
                        title [22180-22236]
                          operator [22180-22182] ("- ")
                          text [22182-22235] ("должен ли быть вызван глобальный мод перед локальным?")
                          newLine [22235-22236]
                      listItem [22236-22293]
                        title [22236-22293]
                          operator [22236-22238] ("- ")
                          text [22238-22292] ("определяет что отображать в modeline когда мод включен")
                          newLine [22292-22293]
                    verbatim [22293-22301]
                      operator [22293-22294] ("=")
                      text [22294-22300] ("global")
                      operator [22300-22301] ("=")
                    text [22301-22302] (" ")
                    verbatim [22302-22311]
                      operator [22302-22303] ("=")
                      text [22303-22310] ("lighter")
                      operator [22310-22311] ("=")
                    text [22311-22312] (" ")
                    headline [22312-22322]
                        :level 2:
                      title [22312-22322]
                        operator [22312-22315] ("** ")
                        text [22315-22321] ("Window")
                        newLine [22321-22322]
                      section [22322-22322]
                    headline [22322-22359]
                        :level 3:
                      title [22322-22359]
                        operator [22322-22326] ("*** ")
                        text [22326-22358] ("Получение ширины текущего экрана")
                        newLine [22358-22359]
                      section [22359-22359]
                    verbatim [22359-22381]
                      operator [22359-22360] ("=")
                      text [22360-22380] ("(window-total-width)")
                      operator [22380-22381] ("=")
                    newLine [22381-22382]
                    headline [22382-22606]
                        :level 2:
                      title [22382-22418]
                        operator [22382-22385] ("** ")
                        text [22385-22417] ("Асинхронное исполнение. Process.")
                        newLine [22417-22418]
                      section [22418-22606]
                        indent [22418-22438] ("                    ")
                        text [22438-22450] ("\\"WakatimeUI\\"")
                        newLine [22450-22451]
                        indent [22451-22471] ("                    ")
                        text [22471-22490] ("wakatime-ui--buffer")
                        newLine [22490-22491]
                        indent [22491-22511] ("                    ")
                        text [22511-22533] ("(wakatime-find-binary)")
                        newLine [22533-22534]
                        indent [22534-22554] ("                    ")
                        text [22554-22591] ("(plist-get wakatime-ui--command-args ")
                        operator [22591-22592] (":")
                        text [22592-22605] ("today-time)))")
                        newLine [22605-22606]
                    text [22606-22638] ("Создание асинхронного процесаа (")
                    link [22638-22749]
                      operator [22638-22639] ("[")
                      linkUrl [22639-22732]
                        operator [22639-22640] ("[")
                        text [22640-22731] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html")
                        operator [22731-22732] ("]")
                      linkName [22732-22748]
                        operator [22732-22733] ("[")
                        text [22733-22747] ("подробнее тут)")
                        operator [22747-22748] ("]")
                      operator [22748-22749] ("]")
                    newLine [22749-22750]
                    newLine [22750-22751]
                    srcBlock [22751-22812]
                      blockHeader [22751-22773]
                        keyword [22751-22773]
                          text [22751-22762] ("#+begin_src")
                          text [22762-22773] (" emacs-lisp")
                      newLine [22773-22774]
                      blockBody [22774-22802]
                        text [22774-22802] ("(setq process (start-process")
                      newLine [22802-22803]
                      blockFooter [22803-22812]
                        keyword [22803-22812]
                          text [22803-22812] ("#+end_src")
                    newLine [22812-22813]
                    text [22813-22847] ("Чтение выходных данных из процесса")
                    newLine [22847-22848]
                    newLine [22848-22849]
                    srcBlock [22849-22882]
                      blockHeader [22849-22871]
                        keyword [22849-22871]
                          text [22849-22860] ("#+begin_src")
                          text [22860-22871] (" emacs-lisp")
                      newLine [22871-22872]
                      newLine [22872-22873]
                      blockFooter [22873-22882]
                        keyword [22873-22882]
                          text [22873-22882] ("#+end_src")
                    newLine [22882-22883]
                  section [21944-22206]
                    indent [21944-21946] ("  ")
                    text [21946-21997] ("\\"Wakatime ui mode. Add time track to doom modeline.")
                    newLine [21997-21998]
                    keyword [21998-22003]
                      text [21998-22003] ("TODO:")
                    newLine [22003-22004]
                    text [22004-22046] ("Add support for other modeline in future.\\"")
                    newLine [22046-22047]
                    indent [22047-22049] ("  ")
                    blockProperty [22049-22064]
                      text [22049-22060] (":init-value")
                      text [22060-22064] (" nil")
                    newLine [22064-22065]
                    indent [22065-22067] ("  ")
                    blockProperty [22067-22076]
                      text [22067-22074] (":global")
                      text [22074-22076] (" t")
                    newLine [22076-22077]
                    indent [22077-22079] ("  ")
                    blockProperty [22079-22091]
                      text [22079-22087] (":lighter")
                      text [22087-22091] (" nil")
                    newLine [22091-22092]
                    indent [22092-22094] ("  ")
                    blockProperty [22094-22113]
                      text [22094-22100] (":group")
                      text [22100-22113] (" 'wakatime-ui")
                    newLine [22113-22114]
                    indent [22114-22116] ("  ")
                    text [22116-22136] ("(if wakatime-ui-mode")
                    newLine [22136-22137]
                    indent [22137-22143] ("      ")
                    text [22143-22168] ("(wakatime-ui--watch-time)")
                    newLine [22168-22169]
                    indent [22169-22173] ("    ")
                    text [22173-22205] ("(wakatime-ui--stop-watch-time)))")
                    newLine [22205-22206]
            headline [23145-23774]
                :level 2:
              title [23145-23156]
                operator [23145-23148] ("** ")
                text [23148-23155] ("Keymaps")
                newLine [23155-23156]
              section [23156-23774]
                headline [23156-23774]
                    :level 3:
                  title [23156-23256]
                    operator [23156-23160] ("*** ")
                    text [23160-23182] ("Создание своего keymap")
                    newLine [23182-23183]
                    srcBlock [23183-23244]
                      blockHeader [23183-23200]
                        keyword [23183-23200]
                          text [23183-23194] ("#+begin_src")
                          text [23194-23200] (" elisp")
                      newLine [23200-23201]
                      blockBody [23201-23234]
                        text [23201-23234] ("(with-current-buffer \\"*Messages*\\"")
                      newLine [23234-23235]
                      blockFooter [23235-23244]
                        keyword [23235-23244]
                          text [23235-23244] ("#+end_src")
                    newLine [23244-23245]
                    keyword [23245-23255]
                      text [23245-23255] ("#+RESULTS:")
                    newLine [23255-23256]
                  section [23183-23701]
                    indent [23183-23185] ("  ")
                    text [23185-23204] ("(read-only-mode -1)")
                    newLine [23204-23205]
                    indent [23205-23207] ("  ")
                    text [23207-23222] ("(erase-buffer))")
                    newLine [23222-23223]
                    newLine [23223-23224]
                    text [23224-23263] ("(setq my-mode-map (make-sparse-keymap))")
                    newLine [23263-23264]
                    text [23264-23318] ("(define-key my-mode-map (kbd \\"C-c C-'\\") 'my-mode-cmd1)")
                    newLine [23318-23319]
                    text [23319-23373] ("(define-key my-mode-map (kbd \\"C-c C-b\\") 'my-mode-cmd2)")
                    newLine [23373-23374]
                    text [23374-23428] ("(define-key my-mode-map (kbd \\"C-c C-c\\") 'my-mode-cmd3)")
                    newLine [23428-23429]
                    text [23429-23485] ("(define-key my-mode-map (kbd \\"<mouse-1>\\") 'my-mode-cmd4)")
                    newLine [23485-23486]
                    text [23486-23560] (";; by convention, major mode's keys should begin with the form C-c C-‹key›")
                    newLine [23560-23561]
                    newLine [23561-23562]
                    text [23562-23588] (";; (dolist (m my-mode-map)")
                    newLine [23588-23589]
                    text [23589-23616] (";;   (message \\"key: %s\\" m))")
                    newLine [23616-23617]
                    newLine [23617-23618]
                    newLine [23618-23619]
                    newLine [23619-23620]
                    newLine [23620-23621]
                    newLine [23621-23622]
                    text [23622-23648] ("(map-keymap '(lambda (v g)")
                    newLine [23648-23649]
                    indent [23649-23664] ("               ")
                    text [23664-23700] ("(message \\"%s: %s\\" v g)) my-mode-map)")
                    newLine [23700-23701]
            headline [23774-28909]
                :level 2:
              title [23774-23917]
                operator [23774-23777] ("** ")
                text [23777-23782] ("Macro")
                newLine [23782-23783]
                property [23783-23795]
                  text [23783-23795] (":PROPERTIES:")
                newLine [23795-23796]
                property [23796-23813]
                  text [23796-23800] (":ID:")
                  text [23800-23813] (" elisp-macros")
                newLine [23813-23814]
                property [23814-23819]
                  text [23814-23819] (":END:")
                newLine [23819-23820]
                text [23820-23830] ("Подробнее ")
                link [23830-23914]
                  operator [23830-23831] ("[")
                  linkUrl [23831-23908]
                    operator [23831-23832] ("[")
                    text [23832-23907] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Macros.html")
                    operator [23907-23908] ("]")
                  linkName [23908-23913]
                    operator [23908-23909] ("[")
                    text [23909-23912] ("тут")
                    operator [23912-23913] ("]")
                  operator [23913-23914] ("]")
                text [23914-23915] (".")
                newLine [23915-23916]
                newLine [23916-23917]
              section [23783-28775]
                headline [23783-23957]
                    :level 3:
                  title [23783-23872]
                    operator [23783-23787] ("*** ")
                    text [23787-23801] ("Простой макрос")
                    newLine [23801-23802]
                    srcBlock [23802-23854]
                      blockHeader [23802-23824]
                        keyword [23802-23824]
                          text [23802-23813] ("#+begin_src")
                          text [23813-23824] (" emacs-lisp")
                      newLine [23824-23825]
                      blockBody [23825-23844]
                        text [23825-23844] ("(defmacro inc (var)")
                      newLine [23844-23845]
                      blockFooter [23845-23854]
                        keyword [23845-23854]
                          text [23845-23854] ("#+end_src")
                    newLine [23854-23855]
                    keyword [23855-23866]
                      text [23855-23865] ("#+RESULTS:")
                      text [23865-23866] (" ")
                    newLine [23866-23867]
                    fixedWidth [23867-23871]
                      operator [23867-23869] (": ")
                      text [23869-23871] ("11")
                    newLine [23871-23872]
                  section [23802-23887]
                    indent [23802-23804] ("  ")
                    text [23804-23836] ("(list 'setq var (list '1+ var)))")
                    newLine [23836-23837]
                    newLine [23837-23838]
                    text [23838-23856] ("(setq test-var 10)")
                    newLine [23856-23857]
                    text [23857-23886] ("(message \\"%s\\" (inc test-var))")
                    newLine [23886-23887]
                headline [23957-24132]
                    :level 3:
                  title [23957-24132]
                    operator [23957-23961] ("*** ")
                    text [23961-23975] ("Изучить макрос")
                    newLine [23975-23976]
                    text [23976-24014] ("Macroexpand - показывает вывод макроса")
                    newLine [24014-24015]
                    newLine [24015-24016]
                    srcBlock [24016-24093]
                      blockHeader [24016-24038]
                        keyword [24016-24038]
                          text [24016-24027] ("#+begin_src")
                          text [24027-24038] (" emacs-lisp")
                      newLine [24038-24039]
                      blockBody [24039-24083]
                        text [24039-24083] ("(message \\"%s\\" (macroexpand '(inc test-var)))")
                      newLine [24083-24084]
                      blockFooter [24084-24093]
                        keyword [24084-24093]
                          text [24084-24093] ("#+end_src")
                    newLine [24093-24094]
                    keyword [24094-24105]
                      text [24094-24104] ("#+RESULTS:")
                      text [24104-24105] (" ")
                    newLine [24105-24106]
                    fixedWidth [24106-24131]
                      operator [24106-24108] (": ")
                      text [24108-24131] ("(setq test-var (1+ tt))")
                    newLine [24131-24132]
                  section [23976-23976]
                headline [24132-24471]
                    :level 3:
                  title [24132-24371]
                    operator [24132-24136] ("*** ")
                    text [24136-24155] ("Цепочка из макросов")
                    newLine [24155-24156]
                    text [24156-24238] ("Macroexpand отображает только первый макрос, чтобы вызвать всю цепочку используем ")
                    verbatim [24238-24255]
                      operator [24238-24239] ("=")
                      text [24239-24254] ("macroexpand-all")
                      operator [24254-24255] ("=")
                    newLine [24255-24256]
                    newLine [24256-24257]
                    srcBlock [24257-24316]
                      blockHeader [24257-24279]
                        keyword [24257-24279]
                          text [24257-24268] ("#+begin_src")
                          text [24268-24279] (" emacs-lisp")
                      newLine [24279-24280]
                      blockBody [24280-24306]
                        text [24280-24306] ("(defmacro inc2 (var1 var2)")
                      newLine [24306-24307]
                      blockFooter [24307-24316]
                        keyword [24307-24316]
                          text [24307-24316] ("#+end_src")
                    newLine [24316-24317]
                    keyword [24317-24328]
                      text [24317-24327] ("#+RESULTS:")
                      text [24327-24328] (" ")
                    newLine [24328-24329]
                    fixedWidth [24329-24370]
                      operator [24329-24331] (": ")
                      text [24331-24370] ("(progn (setq r (1+ r)) (setq s (1+ s)))")
                    newLine [24370-24371]
                  section [24156-24256]
                    indent [24156-24160] ("    ")
                    text [24160-24208] ("(list 'progn (list 'inc var1) (list 'inc var2)))")
                    newLine [24208-24209]
                    newLine [24209-24210]
                    newLine [24210-24211]
                    text [24211-24255] ("(message \\"%s\\" (macroexpand-all '(inc2 r s)))")
                    newLine [24255-24256]
                headline [24471-24679]
                    :level 3:
                  title [24471-24594]
                    operator [24471-24475] ("*** ")
                    text [24475-24512] ("Пример с более сложными конструкциями")
                    newLine [24512-24513]
                    srcBlock [24513-24575]
                      blockHeader [24513-24535]
                        keyword [24513-24535]
                          text [24513-24524] ("#+begin_src")
                          text [24524-24535] (" emacs-lisp")
                      newLine [24535-24536]
                      blockBody [24536-24565]
                        text [24536-24565] ("(defmacro t-becomes-nil (var)")
                      newLine [24565-24566]
                      blockFooter [24566-24575]
                        keyword [24566-24575]
                          text [24566-24575] ("#+end_src")
                    newLine [24575-24576]
                    keyword [24576-24587]
                      text [24576-24586] ("#+RESULTS:")
                      text [24586-24587] (" ")
                    newLine [24587-24588]
                    fixedWidth [24588-24593]
                      operator [24588-24590] (": ")
                      text [24590-24593] ("nil")
                    newLine [24593-24594]
                  section [24513-24598]
                    indent [24513-24515] ("  ")
                    text [24515-24531] ("\`(if (eq ,var t)")
                    newLine [24531-24532]
                    indent [24532-24539] ("       ")
                    text [24539-24556] ("(setq ,var nil)))")
                    newLine [24556-24557]
                    newLine [24557-24558]
                    text [24558-24597] ("(message \\"%s\\" (t-becomes-nil test-var))")
                    newLine [24597-24598]
                headline [24679-25788]
                    :level 3:
                  title [24679-24717]
                    operator [24679-24683] ("*** ")
                    text [24683-24716] ("Динамическое получение переменной")
                    newLine [24716-24717]
                  section [24717-25788]
                    indent [24717-24718] (" ")
                    link [24718-24839]
                      operator [24718-24719] ("[")
                      linkUrl [24719-24821]
                        operator [24719-24720] ("[")
                        text [24720-24820] ("https://stackoverflow.com/questions/24433035/combining-two-variables-into-one-function-name-in-macro")
                        operator [24820-24821] ("]")
                      linkName [24821-24838]
                        operator [24821-24822] ("[")
                        text [24822-24837] ("Подсмотрено тут")
                        operator [24837-24838] ("]")
                      operator [24838-24839] ("]")
                    newLine [24839-24840]
                    indent [24840-24841] (" ")
                    text [24841-24858] ("Чертовая магия 😮")
                    newLine [24858-24859]
                    newLine [24859-24860]
                    srcBlock [24860-25098]
                      blockHeader [24860-24882]
                        keyword [24860-24882]
                          text [24860-24871] ("#+begin_src")
                          text [24871-24882] (" emacs-lisp")
                      newLine [24882-24883]
                      blockBody [24883-25088]
                        text [24883-25088] ("(setq my-custom-variable \\"Hello, amma variable\\")\\n\\n(defmacro get-with-prefix (var-name)\\n  \`(symbol-value (intern (concatenate 'string \\"my-custom\\" \\"-\\" (symbol-name ',var-name)))))\\n\\n(get-with-prefix variable)")
                      newLine [25088-25089]
                      blockFooter [25089-25098]
                        keyword [25089-25098]
                          text [25089-25098] ("#+end_src")
                    newLine [25098-25099]
                    keyword [25099-25110]
                      text [25099-25109] ("#+RESULTS:")
                      text [25109-25110] (" ")
                    newLine [25110-25111]
                    fixedWidth [25111-25133]
                      operator [25111-25113] (": ")
                      text [25113-25133] ("Hello, amma variable")
                    newLine [25133-25134]
                    newLine [25134-25135]
                    text [25135-25301] ("А теперь из plist, если нет - то из глобального скоупа, это еще большая магия. Да, наверное такое не стоит использовать в реальных проектах, но как же руки чешутся 😍")
                    newLine [25301-25302]
                    newLine [25302-25303]
                    srcBlock [25303-25752]
                      blockHeader [25303-25325]
                        keyword [25303-25325]
                          text [25303-25314] ("#+begin_src")
                          text [25314-25325] (" emacs-lisp")
                      newLine [25325-25326]
                      blockBody [25326-25742]
                        text [25326-25742] ("(setq my-custom-variable \\"Hello, amma variable\\")\\n\\n(setq my-plist-with-prop '(:custom-variable nil :test t))\\n\\n(defmacro get-with-prefix (my-plist var-name)\\n  \`(or (plist-get ,my-plist (symbol-value (intern (concatenate 'string \\":\\" (symbol-name ',var-name)))))\\n       (symbol-value (intern (concatenate 'string \\"my\\" \\"-\\" (symbol-name ',var-name))))))\\n\\n(message \\"%s\\" (get-with-prefix my-plist-with-prop custom-variable))")
                      newLine [25742-25743]
                      blockFooter [25743-25752]
                        keyword [25743-25752]
                          text [25743-25752] ("#+end_src")
                    newLine [25752-25753]
                    keyword [25753-25764]
                      text [25753-25763] ("#+RESULTS:")
                      text [25763-25764] (" ")
                    newLine [25764-25765]
                    fixedWidth [25765-25787]
                      operator [25765-25767] (": ")
                      text [25767-25787] ("Hello, amma variable")
                    newLine [25787-25788]
                headline [25788-26937]
                    :level 3:
                  title [25788-26109]
                    operator [25788-25792] ("*** ")
                    text [25792-25814] ("Передача тела (@body) ")
                    tagList [25814-25824]
                      operator [25814-25815] (":")
                      text [25815-25823] ("noexport")
                      operator [25823-25824] (":")
                    newLine [25824-25825]
                    text [25825-25979] ("Пожалуй самая впечатлаяющая фича (имхо, без нее смысл в макросах бы отпал). Макрос склеивает результаты выполнения функций (подумал для org-mode самое то)")
                    newLine [25979-25980]
                    newLine [25980-25981]
                    srcBlock [25981-26084]
                      blockHeader [25981-26003]
                        keyword [25981-26003]
                          text [25981-25992] ("#+begin_src")
                          text [25992-26003] (" emacs-lisp")
                      newLine [26003-26004]
                      blockBody [26004-26074]
                        text [26004-26074] ("(setq test-var 0)\\n(defmacro for (var from init to final do &rest body)")
                      newLine [26074-26075]
                      blockFooter [26075-26084]
                        keyword [26075-26084]
                          text [26075-26084] ("#+end_src")
                    newLine [26084-26085]
                    keyword [26085-26096]
                      text [26085-26095] ("#+RESULTS:")
                      text [26095-26096] (" ")
                    newLine [26096-26097]
                    fixedWidth [26097-26106]
                      operator [26097-26099] (": ")
                      text [26099-26106] ("HAVA: 3")
                    newLine [26106-26107]
                    newLine [26107-26108]
                    newLine [26108-26109]
                  section [25825-26653]
                    indent [25825-25827] ("  ")
                    text [25827-25847] ("\`(let ((,var ,init))")
                    newLine [25847-25848]
                    indent [25848-25853] ("     ")
                    text [25853-25876] ("(while (<= ,var ,final)")
                    newLine [25876-25877]
                    indent [25877-25884] ("       ")
                    text [25884-25890] (",@body")
                    newLine [25890-25891]
                    indent [25891-25898] ("       ")
                    text [25898-25922] ("(setq ,var (1+ ,var)))))")
                    newLine [25922-25923]
                    newLine [25923-25924]
                    newLine [25924-25925]
                    text [25925-25946] ("(for j from 0 to 4 do")
                    newLine [25946-25947]
                    indent [25947-25952] ("     ")
                    text [25952-25982] ("(setq test-var (+ test-var j))")
                    newLine [25982-25983]
                    indent [25983-25988] ("     ")
                    text [25988-26019] ("(setq test-var (/ test-var 2)))")
                    newLine [26019-26020]
                    newLine [26020-26021]
                    text [26021-26050] ("(message \\"HAVA: %s\\" test-var)")
                    newLine [26050-26051]
                    headline [26051-26653]
                        :level 4:
                      title [26051-26396]
                        operator [26051-26056] ("**** ")
                        text [26056-26063] ("Failed ")
                        tagList [26063-26073]
                          operator [26063-26064] (":")
                          text [26064-26072] ("noexport")
                          operator [26072-26073] (":")
                        newLine [26073-26074]
                        text [26074-26143] ("Пример макроса, чтобы наглядно видеть в орге какая функция что делает")
                        newLine [26143-26144]
                        newLine [26144-26145]
                        srcBlock [26145-26212]
                          blockHeader [26145-26167]
                            keyword [26145-26167]
                              text [26145-26156] ("#+begin_src")
                              text [26156-26167] (" emacs-lisp")
                          newLine [26167-26168]
                          blockBody [26168-26202]
                            text [26168-26202] ("(defmacro pretty-log (&rest body)\\n")
                          newLine [26202-26203]
                          blockFooter [26203-26212]
                            keyword [26203-26212]
                              text [26203-26212] ("#+end_src")
                        newLine [26212-26213]
                        keyword [26213-26224]
                          text [26213-26223] ("#+RESULTS:")
                          text [26223-26224] (" ")
                        newLine [26224-26225]
                        fixedWidth [26225-26307]
                          operator [26225-26227] (": ")
                          text [26227-26307] ("--------------------------------------------------------------------------------")
                        newLine [26307-26308]
                        fixedWidth [26308-26324]
                          operator [26308-26310] (": ")
                          text [26310-26324] ("[(+ 1 12)]: 13")
                        newLine [26324-26325]
                        fixedWidth [26325-26342]
                          operator [26325-26327] (": ")
                          text [26327-26342] ("[(- 44 22)]: 22")
                        newLine [26342-26343]
                        fixedWidth [26343-26373]
                          operator [26343-26345] (": ")
                          text [26345-26373] ("[(+ (/ 12 2) (* 33 4))]: 138")
                        newLine [26373-26374]
                        fixedWidth [26374-26395]
                          operator [26374-26376] (": ")
                          text [26376-26395] ("[(setq ttt 12)]: 12")
                        newLine [26395-26396]
                      section [26074-26331]
                        indent [26074-26076] ("  ")
                        text [26076-26116] ("(let ((res (concat (make-string 80 ?-) \\"")
                        newLine [26116-26117]
                        text [26117-26121] ("\\")))")
                        newLine [26121-26122]
                        indent [26122-26126] ("    ")
                        text [26126-26142] ("(dolist (f body)")
                        newLine [26142-26143]
                        indent [26143-26149] ("      ")
                        text [26149-26188] ("(setq res (concat res (format \\"[%s]: %s")
                        newLine [26188-26189]
                        text [26189-26205] ("\\" f (eval f)))))")
                        newLine [26205-26206]
                        indent [26206-26210] ("    ")
                        text [26210-26225] ("(message res)))")
                        newLine [26225-26226]
                        newLine [26226-26227]
                        text [26227-26247] ("(pretty-log (+ 1 12)")
                        newLine [26247-26248]
                        indent [26248-26260] ("            ")
                        text [26260-26269] ("(- 44 22)")
                        newLine [26269-26270]
                        indent [26270-26282] ("            ")
                        text [26282-26303] ("(+ (/ 12 2) (* 33 4))")
                        newLine [26303-26304]
                        indent [26304-26316] ("            ")
                        text [26316-26330] ("(setq ttt 12))")
                        newLine [26330-26331]
                headline [26937-28775]
                    :level 3:
                  title [26937-27313]
                    operator [26937-26941] ("*** ")
                    text [26941-27014] ("Модификация plist через список динамических аргументов как в use-package ")
                    tagList [27014-27024]
                      operator [27014-27015] (":")
                      text [27015-27023] ("noexport")
                      operator [27023-27024] (":")
                    newLine [27024-27025]
                    srcBlock [27025-27088]
                      blockHeader [27025-27047]
                        keyword [27025-27047]
                          text [27025-27036] ("#+begin_src")
                          text [27036-27047] (" emacs-lisp")
                      newLine [27047-27048]
                      blockBody [27048-27078]
                        text [27048-27078] ("(setq res \\"\\")\\n(setq test-alist")
                      newLine [27078-27079]
                      blockFooter [27079-27088]
                        keyword [27079-27088]
                          text [27079-27088] ("#+end_src")
                    newLine [27088-27089]
                    keyword [27089-27100]
                      text [27089-27099] ("#+RESULTS:")
                      text [27099-27100] (" ")
                    newLine [27100-27101]
                    fixedWidth [27101-27114]
                      operator [27101-27103] (": ")
                      text [27103-27114] ("((mode nil)")
                    newLine [27114-27115]
                    fixedWidth [27115-27126]
                      operator [27115-27117] (": ")
                      text [27117-27126] (" (js-mode")
                    newLine [27126-27127]
                    fixedWidth [27127-27140]
                      operator [27127-27129] (": ")
                      text [27129-27140] ("  (:loggers")
                    newLine [27140-27141]
                    fixedWidth [27141-27159]
                      operator [27141-27143] (": ")
                      text [27143-27159] ("   '(\\"hi there\\")")
                    newLine [27159-27160]
                    fixedWidth [27160-27192]
                      operator [27160-27162] (": ")
                      text [27162-27192] ("   :msg-format-template \\"Hi\\"))")
                    newLine [27192-27193]
                    fixedWidth [27193-27212]
                      operator [27193-27195] (": ")
                      text [27195-27212] (" (typescript-mode")
                    newLine [27212-27213]
                    fixedWidth [27213-27226]
                      operator [27213-27215] (": ")
                      text [27215-27226] ("  (:loggers")
                    newLine [27226-27227]
                    fixedWidth [27227-27263]
                      operator [27227-27229] (": ")
                      text [27229-27263] ("   (\\"console.print\\" \\"console.dbg\\")")
                    newLine [27263-27264]
                    fixedWidth [27264-27312]
                      operator [27264-27266] (": ")
                      text [27266-27312] ("   :msg-format-template \\"\\"HELLO WORLD: %s\\"\\")))")
                    newLine [27312-27313]
                  section [27025-28487]
                    indent [27025-27031] ("      ")
                    text [27031-27066] ("'((js-mode (:loggers '(\\"hi there\\") ")
                    blockProperty [27066-27092]
                      text [27066-27086] (":msg-format-template")
                      text [27086-27092] (" \\"Hi\\" ")
                    blockProperty [27092-27115]
                      text [27092-27109] (":argument-divider")
                      text [27109-27115] (" \\"|\\"))")
                    newLine [27115-27116]
                    indent [27116-27124] ("        ")
                    text [27124-27177] ("(typescript-mode (:loggers '(\\"another on\\", \\"and me\\") ")
                    blockProperty [27177-27205]
                      text [27177-27197] (":msg-format-template")
                      text [27197-27205] (" \\"bee\\"))")
                    newLine [27205-27206]
                    indent [27206-27214] ("        ")
                    text [27214-27216] ("))")
                    newLine [27216-27217]
                    newLine [27217-27218]
                    text [27218-27263] ("(defmacro turbo-log-configure (&rest configs)")
                    newLine [27263-27264]
                    indent [27264-27266] ("  ")
                    text [27266-27306] ("(let* ((strategy (or (plist-get configs ")
                    blockProperty [27306-27327]
                      text [27306-27316] (":strategy)")
                      text [27316-27327] (" 'replace))")
                    newLine [27327-27328]
                    indent [27328-27337] ("         ")
                    text [27337-27361] ("(excluded-keys '(:modes ")
                    operator [27361-27362] (":")
                    text [27362-27372] ("strategy))")
                    newLine [27372-27373]
                    indent [27373-27382] ("         ")
                    text [27382-27408] ("(modes (plist-get configs ")
                    operator [27408-27409] (":")
                    text [27409-27416] ("modes))")
                    newLine [27416-27417]
                    indent [27417-27426] ("         ")
                    text [27426-27441] ("current-config)")
                    newLine [27441-27442]
                    newLine [27442-27443]
                    indent [27443-27447] ("    ")
                    text [27447-27472] ("(dolist (k excluded-keys)")
                    newLine [27472-27473]
                    indent [27473-27479] ("      ")
                    text [27479-27517] ("(setq configs (map-delete configs k)))")
                    newLine [27517-27518]
                    newLine [27518-27519]
                    indent [27519-27523] ("    ")
                    text [27523-27543] ("(dolist (mode modes)")
                    newLine [27543-27544]
                    indent [27544-27550] ("      ")
                    text [27550-27581] ("(unless (assoc mode test-alist)")
                    newLine [27581-27582]
                    indent [27582-27590] ("        ")
                    text [27590-27621] ("(push \`(,mode nil) test-alist))")
                    newLine [27621-27622]
                    newLine [27622-27623]
                    indent [27623-27629] ("      ")
                    text [27629-27691] ("(setq current-config (car (cdr-safe (assoc mode test-alist))))")
                    newLine [27691-27692]
                    newLine [27692-27693]
                    indent [27693-27699] ("      ")
                    text [27699-27725] ("(if (eq strategy 'replace)")
                    newLine [27725-27726]
                    indent [27726-27736] ("          ")
                    text [27736-27765] ("(setq current-config configs)")
                    newLine [27765-27766]
                    newLine [27766-27767]
                    indent [27767-27775] ("        ")
                    text [27775-27813] ("(loop for (k v) on configs by 'cddr do")
                    newLine [27813-27814]
                    indent [27814-27828] ("              ")
                    text [27828-27846] ("(if current-config")
                    newLine [27846-27847]
                    indent [27847-27865] ("                  ")
                    text [27865-27895] ("(plist-put current-config k v)")
                    newLine [27895-27896]
                    indent [27896-27912] ("                ")
                    text [27912-27945] ("(setq current-config \`(,k ,v)))))")
                    newLine [27945-27946]
                    newLine [27946-27947]
                    indent [27947-27953] ("      ")
                    text [27953-27980] ("(message \\"QQQ: %s\\" configs)")
                    newLine [27980-27981]
                    indent [27981-27987] ("      ")
                    text [27987-28013] ("(if (assq mode test-alist)")
                    newLine [28013-28014]
                    indent [28014-28024] ("          ")
                    text [28024-28054] ("(setcdr (assq mode test-alist)")
                    newLine [28054-28055]
                    indent [28055-28073] ("                  ")
                    text [28073-28092] ("\`(,current-config))")
                    newLine [28092-28093]
                    indent [28093-28101] ("        ")
                    text [28101-28149] ("\`(push '(,mode '(,current-config)) ,test-alist))")
                    newLine [28149-28150]
                    indent [28150-28156] ("      ")
                    text [28156-28159] (")))")
                    newLine [28159-28160]
                    newLine [28160-28161]
                    text [28161-28181] ("(turbo-log-configure")
                    newLine [28181-28182]
                    indent [28182-28183] (" ")
                    blockProperty [28183-28224]
                      text [28183-28189] (":modes")
                      text [28189-28224] (" (typescript-mode js2-mode js-mode)")
                    newLine [28224-28225]
                    indent [28225-28226] (" ")
                    text [28226-28229] (";; ")
                    blockProperty [28229-28260]
                      text [28229-28235] (":modes")
                      text [28235-28260] (" (typescript-mode j-mode)")
                    newLine [28260-28261]
                    indent [28261-28262] (" ")
                    text [28262-28265] (";; ")
                    blockProperty [28265-28281]
                      text [28265-28271] (":modes")
                      text [28271-28281] (" (js-mode)")
                    newLine [28281-28282]
                    indent [28282-28283] (" ")
                    blockProperty [28283-28300]
                      text [28283-28292] (":strategy")
                      text [28292-28300] (" replace")
                    newLine [28300-28301]
                    newLine [28301-28302]
                    indent [28302-28303] (" ")
                    blockProperty [28303-28343]
                      text [28303-28311] (":loggers")
                      text [28311-28343] (" (\\"console.print\\" \\"console.dbg\\")")
                    newLine [28343-28344]
                    indent [28344-28345] (" ")
                    blockProperty [28345-28386]
                      text [28345-28365] (":msg-format-template")
                      text [28365-28386] (" \\"\\"HELLO WORLD: %s\\"\\")")
                    newLine [28386-28387]
                    newLine [28387-28388]
                    text [28388-28455] ("(message \\"-------------------------------------------------------\\")")
                    newLine [28455-28456]
                    text [28456-28486] ("(message \\"%s\\" (pp test-alist))")
                    newLine [28486-28487]
        headline [28909-29341]
            :level 1:
          title [28909-28934]
            operator [28909-28911] ("* ")
            text [28911-28933] ("Создание своего пакета")
            newLine [28933-28934]
          section [28934-29341]
            headline [28934-29096]
                :level 2:
              title [28934-29096]
                operator [28934-28937] ("** ")
                text [28937-28963] ("Проверка ошибок компиляции")
                newLine [28963-28964]
                srcBlock [28964-29095]
                  blockHeader [28964-28980]
                    keyword [28964-28980]
                      text [28964-28975] ("#+begin_src")
                      text [28975-28980] (" bash")
                  newLine [28980-28981]
                  blockBody [28981-29085]
                    text [28981-29085] ("emacs -Q --batch     --eval '(setq byte-compile-error-on-warn t)'     -f batch-byte-compile turbo-log.el")
                  newLine [29085-29086]
                  blockFooter [29086-29095]
                    keyword [29086-29095]
                      text [29086-29095] ("#+end_src")
                newLine [29095-29096]
              section [28964-28964]
            headline [29096-29159]
                :level 2:
              title [29096-29159]
                operator [29096-29099] ("** ")
                text [29099-29109] ("Contribute")
                newLine [29109-29110]
                link [29110-29158]
                  operator [29110-29111] ("[")
                  linkUrl [29111-29157]
                    operator [29111-29112] ("[")
                    text [29112-29156] ("htest-varps://github.com/leotaku/elisp-check")
                    operator [29156-29157] ("]")
                  operator [29157-29158] ("]")
                newLine [29158-29159]
              section [29110-29110]
            headline [29159-29341]
                :level 2:
              title [29159-29341]
                operator [29159-29162] ("** ")
                text [29162-29164] ("CI")
                newLine [29164-29165]
                link [29165-29274]
                  operator [29165-29166] ("[")
                  linkUrl [29166-29250]
                    operator [29166-29167] ("[")
                    text [29167-29249] ("htest-varps://github.com/a13/reverse-im.el/blob/master/.github/workflows/check.yml")
                    operator [29249-29250] ("]")
                  linkName [29250-29273]
                    operator [29250-29251] ("[")
                    text [29251-29272] ("Пример github actions")
                    operator [29272-29273] ("]")
                  operator [29273-29274] ("]")
                newLine [29274-29275]
                link [29275-29340]
                  operator [29275-29276] ("[")
                  linkUrl [29276-29322]
                    operator [29276-29277] ("[")
                    text [29277-29321] ("htest-varps://github.com/leotaku/elisp-check")
                    operator [29321-29322] ("]")
                  linkName [29322-29339]
                    operator [29322-29323] ("[")
                    text [29323-29338] ("Про elisp check")
                    operator [29338-29339] ("]")
                  operator [29339-29340] ("]")
                newLine [29340-29341]
              section [29165-29165]
        headline [29341-30171]
            :level 1:
          title [29341-29349]
            operator [29341-29343] ("* ")
            text [29343-29348] ("Тесты")
            newLine [29348-29349]
          section [29349-30171]
            text [29349-29478] ("Тесты пишутся весьма просто. От части потому что не нужно мокать кучу зависимостей. Функция в большинстве случаев самодостаточна.")
            newLine [29478-29479]
            newLine [29479-29480]
            srcBlock [29480-29571]
              blockHeader [29480-29502]
                keyword [29480-29502]
                  text [29480-29491] ("#+begin_src")
                  text [29491-29502] (" emacs-lisp")
              newLine [29502-29503]
              blockBody [29503-29561]
                text [29503-29561] ("(ert-deftest my-first-test ()\\n  (should (= (+ 10 10) 20)))")
              newLine [29561-29562]
              blockFooter [29562-29571]
                keyword [29562-29571]
                  text [29562-29571] ("#+end_src")
            newLine [29571-29572]
            text [29572-29579] ("Запуск.")
            newLine [29579-29580]
            newLine [29580-29581]
            srcBlock [29581-29684]
              blockHeader [29581-29597]
                keyword [29581-29597]
                  text [29581-29592] ("#+begin_src")
                  text [29592-29597] (" bash")
              newLine [29597-29598]
              blockBody [29598-29674]
                text [29598-29674] ("emacs -batch -l ert -l package.el -l test.el -f ert-run-tests-batch-and-exit")
              newLine [29674-29675]
              blockFooter [29675-29684]
                keyword [29675-29684]
                  text [29675-29684] ("#+end_src")
            newLine [29684-29685]
            undefined [29685-29708]
              blockHeader [29685-29701]
                keyword [29685-29701]
                  text [29685-29701] ("#+BEGIN_{HIDDEN}")
              newLine [29701-29702]
              blockBody [29702-29702]
              blockFooter [29702-29708]
                keyword [29702-29708]
                  text [29702-29708] ("#+END_")
            text [29708-29716] ("{HIDDEN}")
            newLine [29716-29717]
            newLine [29717-29718]
            srcBlock [29718-29781]
              blockHeader [29718-29740]
                keyword [29718-29740]
                  text [29718-29729] ("#+begin_src")
                  text [29729-29740] (" emacs-lisp")
              newLine [29740-29741]
              blockBody [29741-29771]
                text [29741-29771] ("(setq v (dolist (i '(1 2 3 4))")
              newLine [29771-29772]
              blockFooter [29772-29781]
                keyword [29772-29781]
                  text [29772-29781] ("#+end_src")
            newLine [29781-29782]
            keyword [29782-29793]
              text [29782-29792] ("#+RESULTS:")
              text [29792-29793] (" ")
            newLine [29793-29794]
            fixedWidth [29794-29799]
              operator [29794-29796] (": ")
              text [29796-29799] ("nil")
            newLine [29799-29800]
            newLine [29800-29801]
            newLine [29801-29802]
            headline [29802-30171]
                :level 2:
              title [29802-29859]
                operator [29802-29805] ("** ")
                text [29805-29815] ("Check json")
                newLine [29815-29816]
                srcBlock [29816-29848]
                  blockHeader [29816-29838]
                    keyword [29816-29838]
                      text [29816-29827] ("#+begin_src")
                      text [29827-29838] (" emacs-lisp")
                  newLine [29838-29839]
                  blockFooter [29839-29848]
                    keyword [29839-29848]
                      text [29839-29848] ("#+end_src")
                newLine [29848-29849]
                keyword [29849-29859]
                  text [29849-29859] ("#+RESULTS:")
              section [29816-30128]
                indent [29816-29818] ("  ")
                text [29818-29850] ("(let* ((json-object-type 'plist)")
                newLine [29850-29851]
                indent [29851-29860] ("         ")
                text [29860-29883] ("(json-array-type 'list)")
                newLine [29883-29884]
                indent [29884-29893] ("         ")
                text [29893-29916] ("(json-key-type 'string)")
                newLine [29916-29917]
                indent [29917-29926] ("         ")
                text [29926-29982] ("(json (json-read-file web-roam-configuration-file-path))")
                newLine [29982-29983]
                indent [29983-29992] ("         ")
                text [29992-30025] ("(name-to-config (make-hash-table ")
                blockProperty [30025-30039]
                  text [30025-30030] (":test")
                  text [30030-30039] (" 'equal))")
                newLine [30039-30040]
                indent [30040-30049] ("         ")
                text [30049-30068] ("(server-names '()))")
                newLine [30068-30069]
                indent [30069-30073] ("    ")
                text [30073-30094] ("(dolist (config json)")
                newLine [30094-30095]
                indent [30095-30101] ("      ")
                text [30101-30123] ("(message \\"%s\\" config))")
                newLine [30123-30124]
                indent [30124-30126] ("  ")
                text [30126-30127] (")")
                newLine [30127-30128]
        headline [30171-30267]
            :level 1:
          title [29702-29729]
            operator [29702-29704] ("* ")
            text [29704-29728] ("Статический анализ типов")
            newLine [29728-29729]
          section [29729-29798]
            link [29729-29797]
              operator [29729-29730] ("[")
              linkUrl [29730-29766]
                operator [29730-29731] ("[")
                text [29731-29765] ("https://github.com/emacs-elsa/Elsa")
                operator [29765-29766] ("]")
              linkName [29766-29796]
                operator [29766-29767] ("[")
                text [29767-29795] ("Его нет. Зато есть аннотации")
                operator [29795-29796] ("]")
              operator [29796-29797] ("]")
            newLine [29797-29798]
        headline [30267-32210]
            :level 1:
          title [29798-29820]
            operator [29798-29800] ("* ")
            text [29800-29809] ("Временно ")
            tagList [29809-29819]
              operator [29809-29810] (":")
              text [29810-29818] ("noexport")
              operator [29818-29819] (":")
            newLine [29819-29820]
          section [29820-31741]
            srcBlock [29820-29880]
              blockHeader [29820-29842]
                keyword [29820-29842]
                  text [29820-29831] ("#+begin_src")
                  text [29831-29842] (" emacs-lisp")
              newLine [29842-29843]
              blockBody [29843-29870]
                text [29843-29870] ("(message \\"\\"\\\\[line [0-9]]\\"\\")")
              newLine [29870-29871]
              blockFooter [29871-29880]
                keyword [29871-29880]
                  text [29871-29880] ("#+end_src")
            newLine [29880-29881]
            srcBlock [29881-30016]
              blockHeader [29881-29903]
                keyword [29881-29903]
                  text [29881-29892] ("#+begin_src")
                  text [29892-29903] (" emacs-lisp")
              newLine [29903-29904]
              blockBody [29904-30006]
                text [29904-30006] ("(message \\"%s\\" (string-match \\"{\\\\|);?$\\" \\"public replaceNonPrintableCharacters(text: string): string {\\"))")
              newLine [30010-30011]
              blockFooter [30011-30020]
                keyword [30011-30020]
                  text [30007-30016] ("#+end_src")
            newLine [30016-30017]
            keyword [30017-30028]
              text [30017-30027] ("#+RESULTS:")
              text [30027-30028] (" ")
            newLine [30028-30029]
            fixedWidth [30029-30033]
              operator [30029-30031] (": ")
              text [30031-30033] ("59")
            newLine [30033-30034]
            newLine [30034-30035]
            newLine [30035-30036]
            srcBlock [30036-30217]
              blockHeader [30036-30058]
                keyword [30036-30058]
                  text [30036-30047] ("#+begin_src")
                  text [30047-30058] (" emacs-lisp")
              newLine [30058-30059]
              blockBody [30059-30207]
                text [30059-30207] ("(setq turbo-log--ecmascript-final-symbols '(?; ?)))\\n(while (or (not (eobp)) (member ?) '(?; ?))))\\n                 (setq current-char char-after))))")
              newLine [30207-30208]
              blockFooter [30208-30217]
                keyword [30208-30217]
                  text [30208-30217] ("#+end_src")
            newLine [30217-30218]
            srcBlock [30218-31239]
              blockHeader [30218-30240]
                keyword [30218-30240]
                  text [30218-30229] ("#+begin_src")
                  text [30229-30240] (" emacs-lisp")
              newLine [30240-30241]
              blockBody [30241-31229]
                text [30241-31229] ("(setq quicktype-mode-configs '((\\"go\\" go-mode \\"\\")\\n                               (\\"ts\\" typescript-mode \\"\\")\\n                               (\\"js\\" js2-mode \\"\\")\\n                               (\\"rs\\" rust-mode \\"\\")\\n                               (\\"c++\\" c++(\\"c++\\" c++-mode \\"\\")\\n                               (\\"javascript-prop-types\\" js2-mode \\"\\")\\n                               (\\"flow\\" flow-js2-mode \\"\\")\\n                               (\\"swift\\" swift-mode \\"\\")\\n                               (\\"kotlin\\" kotlin-mode \\"\\")\\n                               (\\"elm\\" elm-mode \\"\\")\\n                               (\\"ruby\\" ruby-mode \\"\\")\\n                               (\\"dart\\" dart-mode \\"\\")\\n                               (\\"py\\" python-mode \\"--python-version 3.7\\")\\n                               (\\"haskell\\" haskell-mode \\"\\")))\\n\\n;; (message \\"%s\\" quicktype-mode-configs)\\n(message \\"%s\\" (cl-rassoc 'go-mode quicktype-mode-configs :test #'member))\\n;; (message \\"%s\\" (cl-rassoc \\"Red Pine\\" needles-per-cluster :test #'member))")
              newLine [31229-31230]
              blockFooter [31230-31239]
                keyword [31230-31239]
                  text [31230-31239] ("#+end_src")
            newLine [31239-31240]
            keyword [31240-31251]
              text [31240-31250] ("#+RESULTS:")
              text [31250-31251] (" ")
            newLine [31251-31252]
            fixedWidth [31252-31267]
              operator [31252-31254] (": ")
              text [31254-31267] ("(go go-mode )")
            newLine [31267-31268]
            newLine [31268-31269]
            newLine [31269-31270]
            srcBlock [31270-31493]
              blockHeader [31270-31292]
                keyword [31270-31292]
                  text [31270-31281] ("#+begin_src")
                  text [31281-31292] (" emacs-lisp")
              newLine [31292-31293]
              blockBody [31293-31483]
                text [31293-31483] ("(setq needles-per-cluster\\n      '((2 \\"Austrian Pine\\" \\"Red Pine\\")\\n        (3 \\"Pitch Pine\\")\\n        (5 \\"White Pine\\")))\\n\\n(message \\"%s\\" (cl-rassoc \\"Red Pine\\" needles-per-cluster :test #'member))")
              newLine [31483-31484]
              blockFooter [31484-31493]
                keyword [31484-31493]
                  text [31484-31493] ("#+end_src")
            newLine [31493-31494]
            keyword [31494-31505]
              text [31494-31504] ("#+RESULTS:")
              text [31504-31505] (" ")
            newLine [31505-31506]
            fixedWidth [31506-31534]
              operator [31506-31508] (": ")
              text [31508-31534] ("(2 Austrian Pine Red Pine)")
            newLine [31534-31535]
            newLine [31535-31536]
            newLine [31536-31537]
            srcBlock [31537-31685]
              blockHeader [31537-31559]
                keyword [31537-31559]
                  text [31537-31548] ("#+begin_src")
                  text [31548-31559] (" emacs-lisp")
              newLine [31559-31560]
              blockBody [31560-31675]
                text [31560-31675] ("(message \\"%s\\" (string-match \\"\\\\({\\\\|;[\\\\w\\\\[:digit]]$\\\\)\\\\|\\\\(const [\\\\w\\\\[:digit]]+ = [\\\\d[:digit:]+$\\\\)\\" \\"  const foo = 1\\"))")
              newLine [31675-31676]
              blockFooter [31676-31685]
                keyword [31676-31685]
                  text [31676-31685] ("#+end_src")
            newLine [31685-31686]
            keyword [31686-31697]
              text [31686-31696] ("#+RESULTS:")
              text [31696-31697] (" ")
            newLine [31697-31698]
            fixedWidth [31698-31703]
              operator [31698-31700] (": ")
              text [31700-31703] ("nil")
            newLine [31703-31704]
            indent [31704-31720] ("                ")
            text [31720-31723] ("i))")
            newLine [31723-31724]
            text [31724-31740] ("(message \\"%s\\" v)")
            newLine [31740-31741]
      "
    `);
  });
});
