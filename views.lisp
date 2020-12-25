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

(defmacro defpage-with-timeline (name lambda-list defpage-keywords &body body)
  `(defpage ,name ,lambda-list ,defpage-keywords
     (:div :class "timeline-page-container"
           (:div )
           (:div :class "main-content-panel"
            ,@body)
           (view/timeline)
           (:div))))


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
           (light "#DEDEDE")
           (margin 10px)
           (unmargin -10px))

     (body
      :background-color #(medium-dark)
      :color #(medium-light)
      :padding 0
      :font-size 16px)

     ((:or a p div h1 h2 h3 h4 pre input textarea ul)
      :line-height 1.6
      :margin #(margin)
      )
     
     (a
      :text-decoration none
      :color #(secondary-color))

     ((:and a :hover)
      :color #(tertiary-color))


     (.post-taglist
      :list-style-type none)

     (.tag
      :color #(primary-color))

     (.button
      :margin-left #(margin)
      :margin-right #(margin)
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
      :padding 10px
      :border-radius 5px
      :background-color #(darkest)
      :color #(secondary-color))

     (blockquote
      :border-left 5px solid #(medium))

      (.time
       :color #(medium-light))

      (.username
       :color #(primary-color))

      (.nav
       :padding 5px

       (span
       (a :display inline) ))
      
      (.right
       :float right)

      
     (.comment
      :border 1px solid #(medium)
      :border-radius 3px
      :color #(light)

      
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


     (.attachment
      :padding 2px
      ((:or img video audio)
       :max-width 200px
       :max-height 200px))

     (.attachments-list
      (ul
       :display flex
       :list-style-type none
       :margin-left #(unmargin)))
     
     (.topic-listing
      :width 100%
      :display grid
      :grid-template-columns 1fr 1fr
      :grid-column-gap 1px
      :grid-row-gap 1px
      (div
       :margin 0px
       :padding 6px
       :padding-left 0px
       :background-color #(dark)))
     
     (.post-listing
      :list-style-type none
      :margin-left #(unmargin)
      (li
       :border-top 1px solid #(medium)
       :background-color #(dark)
       :padding 6px

       (p
        :padding 0
        :margin 0)
       
       (h4
        :margin 0
        :margin-left #(unmargin)
        :padding 0)
       ))

     (.postbody
      :background-color #(light )
      :color #(darkest)
      :border-radius 3px
      :padding 10px

      ((:or h1 h2 h3 h4)
       :margin-top 1.2em
       :color #(dark))
      )

     (.timeline-list
      :margin-left #(unmargin)
      :list-style-type none
      :font-size smaller

      (li
       :padding 4px 
       :margin-left #(unmargin)
       :border-top 1px solid #(medium))

      (a
       :margin-left #(unmargin))
      )

     (.timeline-page-container
      :width 100%
      :display grid
      :grid-template-columns 1fr 5fr 2fr 1fr

      (.main-content-panel

       )

      (.timeline-panel
       :background-color #(dark)))
     )))


(defpage login () (:title "Compost - Login")
  (:form
   :method "POST" :action "/login"
   (:input :placeholder "Username" :type "text" :name "username")
   (:input :placeholder "Password" :type "password" :name "password")
   (:button :type "submit" :class "button" "Login")))


(defview nav (&rest breadcrumbs)
  (:a :class "right"
      :href "/user/profile"
      "Profile")
  (:nav
   :class "nav"
   (:a :href "/" "Frontpage")
   (loop :for (path text) :in breadcrumbs
         :do (:span "→" (:a :href path text)))
   ))

(defpage new-post (topic) ()
  (view/nav)
  (:div
   :class "new-post"
   (:h2 "New Post")
   (:p "Presently, the body of the post is in markdown")
   (:form
    :method "POST"
    :action (format nil "/topic/new-post/~a" (db:store-object-id topic)) 
    :enctype "multipart/form-data"
    (:input :name "title" :placeholder "Post Title") (:br)
    (:textarea :name "text" :rows "22" :cols "80" :wrap "soft")  (:br)
    (:input :name "tags" :placeholder "Tag One, Tag2, ....") (:br)
    (:span :class "button" :id "attachment-button" "Attach File")
    (:div :id "attachment-section")
    (:button :class "button" :type "submit" "Submit Post")))

  (:script
   (ps:ps
     (let ((attachment-count 0))

       (defun make-attachment-form ()
         (let ((input (ps:chain document (create-element "input"))))
           (setf (ps:chain input type) "file"
                 (ps:chain input name) (+  "file-"  (incf attachment-count)))
           input))

       (defun add-attachment ()
         (ps:chain document
                   (get-element-by-id "attachment-section")
                   (append-child (make-attachment-form))))
       
       (ps:chain document
                 (get-element-by-id "attachment-button")
                 (add-event-listener "click" #'add-attachment))))))

(defpage edit-post (post) ()
  (with-slots (db::id title text tags) post 
    (view/nav)
    (:div
     :class "edit-post"
     (:form
      :method "POST"
      :action (format nil "/post/edit/~a" db::id) 
      :enctype "multipart/form-data"
      (:input :name "title" :placeholder "Post Title" :value title) (:br)
      (:textarea
       :name "text" :rows "22" :cols "80" :wrap "soft"
       text)
      (:br)
      (:input :name "tags" :placeholder "Tag One, Tag2, ...."
              :value (join-strings ", " (mapcar 'symbol-name tags))) (:br)
      ;(:span :class "button" :id "attachment-button" "Attach File")
      ;(:div :id "attachment-section")
      (:button :class "button" :type "submit" "Update Post"))))

  )



(defpage-with-timeline topic (topic) (:title (format nil "Compost - ~a"
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
     (when (post-tags post)
       (:div
        :class "post-taglist"
        (:span "tags:")
        (dolist (tag (mapcar 'symbol-name (post-tags post)))
          (:a
           :class "tag"
           :href (format nil "/post/tag-browse?tags=~a" (urlencode:urlencode tag))
           (string-downcase tag)))))     
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

(defpage-with-timeline frontpage () (:title "Compost")
  (view/nav)

  (:h2 "Sections")
    (view/add-topic)
  (:div :class "topic-listing"
        (dolist (topic (all-topics))
          (view/topic topic))))

(defview attachments (post)
  (when-let (attachments (post-attachments post))
    (:div
     :class "attachments-list"
     (:h4 "Attachemnts:")
     (:ul 
      (dolist (attachment (post-attachments post))
        (:li  :class "attachment" (view/file attachment)))))))

(defun image-blob-p (blob)
  (member (db:blob-mime-type blob)
          '("image/png" "image/jpg" "image/svg"
            "image/jpg" "image/jpeg" "image/bmp"
            "image/gif")
          :test #'string-equal))

(defun video-blob-p (blob)
  (member (db:blob-mime-type blob)
          '("video/webm" "video/mp4" "video/ogg")
          :test #'string-equal))

(defun audio-blob-p (blob)
  (member (db:blob-mime-type blob)
          '("audio/wav" "audio/webm" "audio/ogg" "audio/mp3")
          :test #'string-equal))


(defview file (attachment)
  (let ((url (path-to attachment)))
    (cond
      ((image-blob-p attachment)
       (:a :href url (:img :src url)))
      ((video-blob-p attachment)
       (:video
        :controls "true"
        (:source :src url 
                 :type (db:blob-mime-type attachment))))
      ((audio-blob-p attachment)
       (:audio
        :controls "true"
        (:source :src url )))

      (t
       (:a :href url (attachment-filename attachment))))))


(defpage-with-timeline post (post) (:title (format nil "Compost - ~a"
                                     (post-title post)))
  (view/nav (list (path-to (post-topic post))
                  (topic-name (post-topic post)))
            (list (path-to post)
                  (post-title post)))
  (:div
   :class "postbody"
   (render-post post)
   (view/attachments post))
  (when (post-tags post)
    (:div
     :class "post-taglist"
     (:span "tags: ")
     (dolist (tag (mapcar 'symbol-name (post-tags post)))
       (:a
        :class "tag"
        :href (format nil "/post/tag-browse?tags=~a" (urlencode:urlencode tag))
        (string-downcase tag)))))

  (:div
   (:span :class "time"
          (timestring (post-created post)
                      (user-timezone *user*)))
   " -- "
   (:span :class "username"
          (user-name (post-user post)))

   

   (when (eql (post-user post) *user*)
     (:a :class "button"
         :href (format nil "/post/edit/~a" (db:store-object-id post))
         "Edit"))
   
   )
  (view/reply-form post)
  (:h4 "comments")
  (dolist (reply (sorted-replies-to post))
    (view/comment reply)))

(defun query->plist (qstring)
  (when qstring
    (loop :for kv :in (split-sequence:split-sequence #\& (urlencode:urldecode qstring))
          :appending (destructuring-bind (k v) (split-sequence:split-sequence #\= kv)
                       (list (make-keyword k) v)))))

(defun query-plist ()
  (query->plist (getf *req* :query-string)))


(defpage-with-timeline tag-browse ()  (:title "Compost - Tag Browse ")
  (let* ((tag-string (getf (query-plist) :tags))
         (tags (parse-tags tag-string)))
    (view/nav)
    (:p "Posts tagged with: " tag-string)
    (:ul :class "post-listing"
        (dolist (post (apply 'posts-by-tag tags))
          (view/title-post post)))))

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
   :id (format nil "comment-~a" (db:store-object-id comment-post))
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

(defview timeline ()
  (:div
   :class "timeline-panel"
   (:h2 "Timeline")
   (:ul
    :class "timeline-list"
    (dolist (post (most-recent-posts))
      (:li
       (:p 
        (:a :href (path-to post)
            (typecase post
              (reply-post
               (let ((root (find-root-post post))) 
                 (:span "replying to "
                        (post-title root)
                        " in "
                        (topic-name (post-topic root)))))
              (title-post
               (:span "created "
                      (post-title post)
                      " in "
                      (topic-name (post-topic post))))))
        (:br)
        (:span :class "time"
               (short-timestring (post-created post) (user-timezone *user*)))
        " --  "
        (:span :class "username"
               (user-name (post-user post))))
       (:p ))))
   ))

(defpage profile () ()
    (with-slots (name ) *user*
      (view/nav)
      (:form
       :method "POST"
       :action "/user/name"
       (:input :placeholder "Change Username"
               :value name
               :name "name")
       (:button :type "submit" :class "button" "Update Name"))
      
      (:form
       :method "POST"
       :action "/user/password"
       (:input :placeholder "Current Password"  :type "password"
               :name "old-password")
       (:input :placeholder "New Password" :type "password"
               :name "new-password")
       (:input :placeholder "Repeat New Password" :type "password"
               :name "repeat-password")
       (:button :type "submit" :class "button" "Change Password"))

      

      ))
