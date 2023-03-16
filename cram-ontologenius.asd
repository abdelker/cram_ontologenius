(defsystem cram-ontologenius
  :depends-on (
             :roslisp
	           :actionlib_msgs-msg
	           :actionlib
	           :geometry_msgs-msg
             :cl-transforms
             :cl-transforms-stamped
             :cl-tf
             :cl-tf2
             :cram-tf
	           :cram-language
             :cram-designators 
	           :cram-prolog
             :cram-process-modules 
	           :cram-language-designator-support
             :cram-executive 
	           :cram-cloud-logger
             :ontologenius-srv
             :ontologenius-msg
	           )

  :components
  ((:module "src"
            :components
            (
              (:file "package")
              (:file "client-base" :depends-on ("package"))
              (:file "manager-client" :depends-on ("package"
                                                   "client-base"))
             ;; (:file "feed-publisher" :depends-on ("package"))
              (:file "ontology-manipulator" :depends-on ("package"
                                                        ;; "feed-publisher"
                                                         ))
              (:file "ontologies-manipulator" :depends-on ("package"
                                                         "ontology-manipulator"
                                                         "manager-client"))                                       
             )))
  )

