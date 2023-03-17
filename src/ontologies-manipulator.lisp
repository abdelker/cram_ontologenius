(in-package :onto)

(defparameter *manipulators* (make-hash-table))

;; (defclass ontologies-manipulator (manager-client)
;;     (
;;         (manipulators)
;;         )
;;     )

(defun init-ontologies-man ()
    "Constructs a manipulator for several instances of ontologies."

    (init-manager)
)

(defun wait-init (&optional (timeout -1))
    "Wait for ontologenius services to be advertised and available for. Blocks until it is.
    timeout(int) is the amount of time to wait for before timing out.
    If timeout is -1 (default), waits until the node is shutdown."

    (cond 
        ((not (eql timeout -1))
        
            (roslisp:wait-for-service "ontologenius/manage" timeout))
        
        
        ((roslisp:wait-for-service "ontologenius/manage"))
        
        )

)

(defun add-onto (name)
    "Creates a new instance of ontologyManipulator identified by the name name(str).
    Returns False if the creation fails. Returns True even if the instance already exists."

    (let ((onto-man (make-instance 'onto-man :name name)))
            ;;(print onto-man)

        (if (gethash name *manipulators*)
                (values t)
                
                (if (eq (add-inst-onto name) nil)
                    (values nil)

                    (setf (gethash name *manipulators*) onto-man)
                ))

            ;; ((cond 
            ;;     ((eq (add-inst-onto name) nil)
            ;;         (values nil))))

                ;;(((setf (gethash name *manipulators*) onto-man)))
                )
)

