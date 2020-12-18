(in-package #:compost)

(defmacro defpage  (name lambda-list (&key stylesheets scripts (title "")) &body body)
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


(defview new-post (topic)
  (:form
   :method "POST"
   :action (format nil "/topic/new-post/~a" (db:store-object-id topic)) 
   (:input :name "title" :placeholder "Post Title")
   (:textarea :name "text" :rows "12" :cols "60")
   (:button :class "button" :type "submit" "Submit Post")))

(defpage topic (topic) (:title (format nil "Compost - ~a"
                                       (topic-name topic)))
  (view/nav)
  (:h1 (topic-name topic))
  (view/new-post topic)
  (dolist (post (title-posts-by-topic topic))
    (view/title-post post)))

(defview topic (topic)
  (:div
   :class "topic listing"
   (:a :href (path-to topic) (topic-name topic))))


(defview add-topic ()
  (:form
   :method "POST"
   :action "/topic/add"
   (:input :name "name" :placeholder "New Topic")
   (:button
    :class "button"
    :type "submit"
    "Add")))

(defpage frontpage () (:title "Compost")
  (view/nav)
  (:h1 "Hey " (user-name *user*))
  (dolist (topic (all-topics))
    (view/topic topic))
  (view/add-topic))

(defpage post (post) (:title (format nil "Compost - ~a"
                                     (post-title post)))
  (view/nav (list (path-to (post-topic post))
                  (topic-name (post-topic post))))
  (view/title-post post)
  (:div
   :class "postbody post"
   (render-post post)))
