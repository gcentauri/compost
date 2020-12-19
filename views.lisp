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
           (dark "#25303B")
           (medium-dark "#324150")
           (medium "#4D637A")
           (medium-light "#5E7A96")
           (light "#DDD"))

     (body
      :background-color #(medium-dark)
      :color #(light)
      :padding 0
      :font-size 16px
      )

     ((:or a p div h1 h2 h3 h4 pre)

      :max-width 650px
      :line-height 1.4
      :margin 20px)
     
     (a
      :text-decoration none
      :color #(primary-color))

     ((:and a :hover)
      :color #(tertiary-color))

     (.button
      :background-color #(medium-dark)
      :padding 8px
      :border-radius 4px
      :color #(tertiary-color)
      :border solid 1px #(tertiary-color))

     ((:and  .button :hover)
      :background-color #(dark))

     (input
      :background-color #(medium)
      :border none
      :color #(light)
      :padding 4px
      :border-radius 4px)

     (pre
      :background-color #(dark)
      
      :color #(secondary-color))

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
         :do (:a :href path text))))

(defview title-post (post)
  (with-slots (title created user) post
    (:div
     :class "post listing"
     (:a :href (path-to post) (:h3 title))
     (:span :class "time"
            (timestring (post-created post)
                        (user-timezone *user*)))
     (:span :class "username"
            (user-name user))
     

     (:div
      :class "attachments"
      (dolist (attachment (post-attachments post))
        (:a :href (path-to attachment) (attachment-filename attachment)))))))


(defpage new-post (topic) ()
  (:form
   :method "POST"
   :action (format nil "/topic/new-post/~a" (db:store-object-id topic)) 
   (:input :name "title" :placeholder "Post Title")
   (:textarea :name "text" :rows "12" :cols "60" :wrap "soft")
   (:button :class "button" :type "submit" "Submit Post")))

(defpage topic (topic) (:title (format nil "Compost - ~a"
                                       (topic-name topic)))
  (view/nav)
  (:h1 (topic-name topic))
  (:a :href (format nil "/topic/new-post/~a" (db:store-object-id topic))
      :class "button"
      "New Post")
  (dolist (post (posts-by-topic topic))
    (view/title-post post)))

(defview topic (topic)
  (:div
   :class "topic listing"
   (:a :href (path-to topic) (topic-name topic))))


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
  (dolist (topic (all-topics))
    (view/topic topic)))

(defpage post (post) (:title (format nil "Compost - ~a"
                                     (post-title post)))
  (view/nav (list (path-to (post-topic post))
                  (topic-name (post-topic post))))
  (view/title-post post)
  (:div
   :class "postbody post"
   (render-post post)))
