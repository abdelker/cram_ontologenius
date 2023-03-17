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
        If timeout is -1 (default), waits until the node is shutdown.
        "
(cond 
    ((not (timeout -1))
    
        (wait-for-service "ontologenius/manage" timeout))
    
    (wait-for-service "ontologenius/manage")
    
    )

)

(defun add (name)
    "Creates a new instance of ontologyManipulator identified by the name name(str).
    Returns False if the creation fails. Returns True even if the instance already exists."
    (let ((onto-man (make-instance 'onto-man :name name)))   
    (cond 
        ((eq (member name 'manipulators) t)
            (return t))
    
        (cond 
            ((eq (add-manage name) nil)
                (nil))
            (((setf (gethash name *manipulators*) onto-man))))

)))