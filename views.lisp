(in-package #:compost)

(defmacro defpage  (name lambda-list
                    (&key (stylesheets (list "/css/main.css")) scripts (title ""))
                    &body body)
  (let ((page-name (intern (format nil "PAGE/~a" name))))
    `(defun ,page-name ,lambda-list
       (with-html-string
         (:doctype)
         (:html
          (:head
           (:title ,title)
           (dolist (css (list ,@stylesheets)) 
             (:link :rel "stylesheet" :href css)))
          (:body
           ,@body
           (dolist (js (list ,@scripts))
             (:script :src js))))))))

(defmacro defview (name lambda-list &body body)
  (let ((view-name (intern (format nil "VIEW/~a" name))))
    `(defun ,view-name ,lambda-list
       (with-html
         ,@body))))


(defun main-css ()
  (lass:compile-and-write
   '(:let ((primary-color "#EAA864")
           (secondary-color "#8DC85F")
           (tertiary-color "#208B8A")
           (darkest "#1a1a1a")
           (dark "#25303B")
           (medium-dark "#324150")
           (medium "#4D637A")
           (medium-light "#CCC")
           (light "#EEE"))

     (body
      :background-color #(medium-dark)
      :color #(light)
      :padding 0
      :font-size 16px
      )

     ((:or a p div h1 h2 h3 h4 pre input textarea ul)
      :max-width 680px
      :line-height 1.4
      :margin 20px)
     
     (a
      :text-decoration none
      :color #(secondary-color))

     ((:and a :hover)
      :color #(tertiary-color))

     (.button
      :margin-left 20px
      :margin-right 20px
      :margin-top 2px
      :margin-bottom 2px
      :background-color #(medium-dark)
      :padding 2px
      :border-radius 4px
      :color #(secondary-color)
      :border solid 1px #(secondary-color))

     ((:and  .button :hover)
      :background-color #(dark)
      :color #(tertiary-color)
      :border solid 1px #(tertiary-color))

     ((:or input textarea)
      :background-color #(medium)
      :border none
      :color #(light)
      :padding 4px
      :border-radius 4px)
     
     (pre
      :background-color #(darkest)
      
      :color #(secondary-color))

      (.time
       :color #(medium-light))

      (.username
       :color #(primary-color))
      
     (.comment
      :border 1px solid #(medium)
      :border-radius 5px
      :color #(light)

      (.right
       :float right
       :text-align right)
      
      (h1
       :font-size 1.3em)

      (h2
       :font-size 1.2em)

      (h3
       :font-size 1.1em)

      ((:or h4 h5 h6)
       :font-size 1.0em))
     
     (.hidden
      :visibility hidden
      :height 0
      :widht 0)

     (.topic-listing
      :width 100%
      :display grid
      :grid-template-columns 1fr 1fr
      :grid-column-gap 1px
      :grid-row-gap 1px
      (div
       :margin 0px
       :padding 10px
       :padding-left 0px
       :background-color #(dark)))
     
     (.post-listing
      :list-style-type none
      :margin-left -20px
      (li
       :border-top 1px solid #(medium)
       :background-color #(dark)
       :padding 10px

       (p
        :padding 0
        :margin 0)
       
       (h4
        :margin 0
        :margin-left -20px
        :padding 0)
       ))

     (.postbody
      :background-color #(dark)
      :border-radius 5px
      :padding 10px
      :padding-left 4px)
     
     )))


(defpage login () (:title "Compost - Login")
  (:form
   :method "POST" :action "/login"
   (:input :placeholder "Username" :type "text" :name "username")
   (:input :placeholder "Password" :type "password" :name "password")
   (:button :type "submit" :class "button" "Login")))


(defview nav (&rest breadcrumbs)
  (:nav
   :class "nav"
   (:a :href "/" "Frontpage")
   (loop :for (path text) :in breadcrumbs
         :do (:span "→" (:a :href path text)))))




(defpage new-post (topic) ()
  (view/nav)
  (:div
   :class "new-post"
   (:h2 "New Post")
   (:p "Presently, the body of the post is in markdown")
   (:form
    :method "POST"
    :action (format nil "/topic/new-post/~a" (db:store-object-id topic)) 
    (:input :name "title" :placeholder "Post Title") (:br)
    (:textarea :name "text" :rows "22" :cols "80" :wrap "soft")
    (:br)
    (:button :class "button" :type "submit" "Submit Post"))))

(defpage topic (topic) (:title (format nil "Compost - ~a"
                                       (topic-name topic)))
  (view/nav (list (path-to topic) (topic-name topic) ))
  (:h1 (topic-name topic))
  (:a :href (format nil "/topic/new-post/~a" (db:store-object-id topic))
      :class "button"
      "New Post")
  (:ul
   :class "post-listing"
   (dolist (post (posts-by-topic topic))
     (view/title-post post))))

(defview title-post (post)
  (with-slots (title created user) post
    (:li 
     (:h4  (:a :href (path-to post) title))
     (:br)
     (:p 
      (:span :class "time"
             (timestring (post-created post)
                         (user-timezone *user*)))
      " -- "
      (:span :class "username"
             (user-name user))))))

(defview topic (topic)
  (:div
   (:a :href (path-to topic)
       (topic-name topic))
   (:span
    :class "right"
    (format nil "~a" (length (posts-by-topic topic)))
    " posts")))


(defview add-topic ()
  (:form
   :method "POST"
   :action "/topic/add"
   (:input :name "name" :placeholder "New Section")
   (:button
    :class "button"
    :type "submit"
    "Add")))

(defpage frontpage () (:title "Compost")
  (view/nav)
  (:h1 "Hey " (user-name *user*))
  (view/add-topic)
  (:h2 "Sections")
  (:div :class "topic-listing"
        (dolist (topic (all-topics))
          (view/topic topic))))

(defview attachments (post)
  (:div
    :class "attachments"
    (dolist (attachment (post-attachments post))
      (:a :href (path-to attachment) (attachment-filename attachment)))))


(defpage post (post) (:title (format nil "Compost - ~a"
                                     (post-title post)))
  (view/nav (list (path-to (post-topic post))
                  (topic-name (post-topic post)))
            (list (path-to post)
                  (post-title post)))
  

  (:div
   :class "postbody"
   (render-post post)
   (view/attachments post))
  (:div
   (:span :class "time"
          (timestring (post-created post)
                      (user-timezone *user*)))
   " -- "
   (:span :class "username"
          (user-name (post-user post))))
  (view/reply-form post)
  (:h4 "comments")
  (dolist (reply (sorted-replies-to post))
    (view/comment reply)))

(defview reply-form (post)
  (let* ((post-id (db:store-object-id post))
         (button-id (format nil "reply-reveal-~a" post-id)))
    (:button :id button-id :class "button" "Reply")

    (:form
     :class "reply-form"
     :class "hidden"
     :method "POST"
     :action (format nil  "/post/reply/~a" post-id)
     (:textarea :name "text" :rows "12" :cols "60" :wrap "soft")
     (:br)
     (:button :type "submit" :class "button" "Reply"))

    (:script
     (ps:ps
       (ps:chain
        document
        (get-element-by-id (ps:lisp button-id))
        (add-event-listener 
         "click"
         (lambda (event)
           (ps:chain event target next-element-sibling class-list (remove "hidden"))
           (ps:chain event target class-list (add "hidden")))))))))

(defview comment (comment-post)
  (:div
   :class "comment"
   (:p 
    (:span :class "time"
           (timestring (post-created comment-post)
                       (user-timezone *user*)))
    " --  "
    (:span :class "username"
           (user-name (post-user comment-post)))
    " says:")
   (render-post comment-post)
   (view/attachments comment-post)

   (view/reply-form comment-post)

   (dolist (reply (sorted-replies-to comment-post))
     (view/comment reply ))))

