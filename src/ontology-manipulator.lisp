(in-package :onto)

(defclass ontology-manipulator ()
    (
        (name
        :initarg :name
        :initform "")
        
        (service-name
        :initarg :service-name
        :initform "ontologenius/sparql")

        individual-client
        action-client
        
        )
    )

(defmethod initialize-instance :before ((onto-man ontology-manipulator) &key)
    """Constructs a ROS client linked to the service name(str)."""
    (let ((name (slot-value onto-man 'name)) (service-name (slot-value onto-man 'service-name)))
            (setf (slot-value onto-man 'individual-client)
                (make-instance 'individual-client :individual-name name))
            (setf (slot-value onto-man 'action-client)
                (make-instance 'action-client :action-name name))
                        ;; ((individuals individual-client)
                        ;; (actions action-client)) onto
                        ;; (format t "got ~a, ~a~&" individuals actions)
                        ;;(setf individuals (make-instance 'individual-client :individual-name name))
                        ;;(setf actions (make-instance 'action-client :action-name name))
                        
        (cond 
            ((not (string= name ""))
                (setq service-name (concatenate 'string  "/" name))))

        (roslisp:wait-for-service service-name)
        (initialize-instance onto-man)
    )
    
)

;; (defun init-ontology-man (&optional (name ""))
;;     """Constructs an ontology manipulator with.
;;         Can be used in a multi-ontology mode by specifying the name of the ontology name(str). For classic use, do not specify the ontology name name."""
;;     (init-indiv-client name)
;;     (init-action-client name)
;;     (let ((service-name "ontologenius/sparql"))
;;         (cond 
;;             ((not (string= name ""))
;;                 (setq service-name (concatenate 'string  "/" name))))

;;         (roslisp:wait-for-service
;;             service-name)
;;     )
;; )

(defun nb-onto ()
    """Gives the total number (int) of service calls from all ROS clients instances since the last reset"""

            (nb)
            )

(defun reset-nb-onto ()
    """Reset the call counter for all instances of ROS clients."""

            (reset-nb)
            )

(defun close-onto ()
    """Same as the ActionClient closing function. Link all the concepts loaded from files and the Internet. Before closing an ontology, exploration requests are not allowed.
        Returns False if the service call fails."""

            (close)
            )

(defun set-verbose-onto (verbose)
    """Same as the ActionClient closing function. Link all the concepts loaded from files and the Internet. Before closing an ontology, exploration requests are not allowed.
        Returns False if the service call fails."""

            (set-verbose verbose)
            )