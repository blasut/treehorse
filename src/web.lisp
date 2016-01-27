(in-package :cl-user)
(defpackage treehorse.web
  (:use :cl
        :caveman2
        :treehorse.config
        :treehorse.view
        :treehorse.db
        :datafly
        :sxql
        :postgres-json)
  (:export :*web*))
(in-package :treehorse.web)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)


; Change the default from-json method.
(defun from-json (string)
  "Parse the JSON string STRING and return the resulting lisp object."
  (yason:parse string))

(defparameter *from-json* #'from-json)

(defun get-all-field-types ()
  '((:value "text" :name "Text input")
    (:value "password" :name "Password")
    (:value "date" :name "Date")
    (:value "date-time" :name "Date time")
    (:value "textarea" :name "Textarea")
    (:value "checkbox" :name "Checkbox")
    (:value "dropdown" :name "Dropdown")
    (:value "radio-button" :name "Radio button")
    (:value "color" :name "Colorpicker")
    (:value "number" :name "Number")
    (:value "range" :name "Range")
    (:value "url" :name "URL")))

(defun get-field (id)
  (with-connection (db)
    (retrieve-one
     (select :*
       (from :fields)
       (where (:= :field_id id))))))

(defun get-all-fields ()
  (with-connection (db)
    (retrieve-all
     (select :*
       (from :fields)))))

(defun aget (list key)
  (cdr (assoc key list :test #'string=)))

(defun save-field (params)
  (with-connection (db)
    (execute
     (insert-into :fields
       (set= :field (aget params "field")
             :type (aget params "type")
             :defaults (aget params "defaults"))))))

(defun update-field (id params)
  (with-connection (db)
    (execute
     (update :fields
       (set= :field   (aget params "field")
             :type   (aget params "type")
             :defaults (aget params "defaults"))
       (where (:= :field_id id))))))

(defun delete-field (id)
  (with-connection (db)
    (execute
     (delete-from :fields
       (where (:= :field_id id))))))

(defun get-all-templates ()
  (with-connection (db)
    (retrieve-all
     (select :*
       (from :templates)))))

(defun serialize-fields (template)
  (let ((fields  (loop for field in (getf template :fields)
                    collect (progn
                              (setf (getf field :defaults) (split-sequence:split-sequence #\newline (getf field :defaults)))
                              field))))
    (setf (getf template :fields) fields)
    template))

(defun get-all-templates-with-fields ()
  (let ((templates (get-all-templates)))
    (loop for template in templates
       collect (serialize-fields (get-template (cadr template))))))

(defun get-template (id)
  (with-connection (db)
    (let ((template (retrieve-one
                     (select :templates.*
                       (from :templates)
                       (where (:= :templates.template_id id)))))
          (fields (retrieve-all
                   (select :*
                     (from :templates_fields)
                     (join :fields :on (:= :fields.field_id :templates_fields.field_id))
                     (where (:= :templates_fields.template_id id))))))
      (list :template template :fields fields))))

(defun get-latest-template ()
  (retrieve-all
   (select :template_id
     (from :templates)
     (order-by (:desc :timestamp))
     (limit 1))))

(defun save-template (data)
  (with-connection (db)
    (execute
     (insert-into :templates
       (set= :template (aget data "template"))))
    (let ((template-id (cadr (first (get-latest-template)))))
      (format *standard-output* "~S" template-id)
      (loop for field in (aget data "fields")
         do (execute
             (insert-into :templates_fields
               (set= :field_id field
                     :template_id template-id)))))))

(defun update-template (id data)
  (format *standard-output* "~S" id)
  (format *standard-output* "~S" data)
  (with-connection (db)
    (execute
     (update :templates
       (set= :template (aget data "template"))
       (where (:= :template_id id))))
    (let ((template-id id))
      (format *standard-output* "~S" template-id)
      (execute
       (delete-from :templates_fields
         (where (:= :template_id id))))
      (loop for field in (aget data "fields")
         do (execute
             (insert-into :templates_fields
               (set= :field_id field
                     :template_id template-id)))))))

(defun delete-template (id)
  (with-connection (db)
    (execute
     (delete-from :templates
       (where (:= :template_id id))))))

(defun get-all-items ()
  (fetch-all -items-))

(defun get-item (id)
  (fetch -items- id))

(defun prepare-item (data)
  (let ((fields (loop for list in (aget data "fields")
                                     collect (list (car (first list))  (cdr (first list))
                                                   (car (second list)) (cdr (second list))
                                                   (car (third list))  (cdr (third list)))))
        (metadata (loop for list in (aget data "metadata")
                     collect (list (car list) (cdr list)))))
    (obj "fields" fields "metadata" metadata)))

(defun save-item (data)
  (insert -items- (prepare-item data)))

(defun update-item (id data)
  (supersede -items- id (prepare-item data)))

(defun delete-item (id)
  (excise -items- id))

;; View function
(defun for-view (list-or-table)
  (if (typep list-or-table 'list)
      (map 'list #'alexandria:hash-table-alist list-or-table)
      (alexandria:hash-table-alist list-or-table)))

;;
;; Routing rules
(defroute "/" ()
  (render #P"index.html"
          (list :items (for-view (get-all-items)))))

;;;;;;;;;;;;;;
;; Fields
;;;;;;;;;;;;;;
(defroute "/fields" ()
  (render #P"fields/index.html"
          (list :fields (get-all-fields))))

(defroute "/fields/new" ()
  (render #P"fields/new.html"
          (list :types (get-all-field-types))))

(defroute ("/fields" :method :POST) (&key _parsed)
  (save-field _parsed)
  "Created")

(defroute "/fields/:id/edit" (&key id)
  (render #P"fields/edit.html"
          (list :field (get-field id)
                :types (get-all-field-types))))

(defroute ("/fields/:id" :method :POST) (&key id _parsed)
  (update-field id (aget _parsed "field"))
  "Updated")

(defroute "/fields/:id/delete" (&key id)
  (delete-field id)
  "Deleted")

;;;;;;;;;;;;;;
;; Templates
;;;;;;;;;;;;;;
(defroute "/templates" ()
  (render #P "templates/index.html"
          (list :templates (get-all-templates))))

(defroute "/templates/new" ()
  (render #P"templates/new.html"
          (list :fields (get-all-fields))))

(defroute ("/templates" :method :POST) (&key _parsed)
  (save-template _parsed)
  "Sparat")

(defroute "/templates/:id/edit" (&key id)
  (render #P "templates/edit.html"
          (list :fields (get-all-fields)
                :template (get-template id))))

(defroute ("/templates/:id" :method :POST) (&key id _parsed)
  (update-template id _parsed)
  "Sparat")


(defroute "/templates/:id/delete" (&key id)
  (delete-template id)
  "Deleted")

;;;;;;;;;;;;;;
;; Items
;;;;;;;;;;;;;;
(defroute "/items/new" ()
  (render #P"items/new.html"
          (list :templates (get-all-templates-with-fields))))

(defroute ("/items/new" :method :POST) (&key _parsed)
  (save-item _parsed)
  "Sparat")

(defroute ("/items/:id") (&key id)
  (render #P "items/show.html"
          (list :item (for-view (get-item id)))))

(defroute ("/items/:id/edit") (&key id)
  (render #P "items/edit.html"
          (list :item (for-view (get-item id)))))

(defroute ("/items/:id" :method :POST) (&key id _parsed)
  (update-item id _parsed)
  "Uppdaterat")

(defroute ("/items/:id/delete") (&key id)
  (delete-item id)
  "Deletad")

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
