(in-package :onto)

(defvar *verbose* nil)
(defvar *cpt* 0)

(defvar *client-srv* nil "ROS service to the ontology client")
(defvar *name* nil)


(defun init-client (name)
"""Constructs a ROS client linked to the service name(str)."""
    (setf *name* name)
    (setf *client-srv* (concatenate 'string "ontologenius/" *name* )))

(defun call-client-srv (action param)
  "Function to call the ontology client service."
  (call-service *client-srv*
                'ontologenius-srv:OntologeniusService
                :action action
                :param param
                )) 

(defun nb ()
"""Gives the total number (int) of service calls from all ClientBase instances since the last reset."""
    (return *cpt*))

(defun reset-nb ()
"""Reset Call Counter for all instances of ClientBase."""
    (setf *cpt* 0))

(defun set-verbose (verbose)
"""If verbose(bool) is set to True, the clients will post messages about
           the failure to call the services and about their restoration.
        """
    (setf *verbose* verbose))

(defun call (action param)
    """Call the service set up in the constructor of ClientBase with the request
    defined with action(str) and param(str) and returns all the results (str[]).
    If the service call fails, the function returns None"""

    (setf *cpt* (+ 1 *cpt*))

    (handler-case 
        (progn 
            (let
                ((values (msg-slot-value (call-client-srv action param) :values)))
                (value values)))

        (roslisp::ros-rpc-error () 
            (cond
                ((eql *verbose* t)
                    (let ((error-message (concatenate 'string "Failure to call ontologenius/" *name* )))
                            (print error-message)))

                ((setf *client-srv* (concatenate 'string "ontologenius/" *name* ))))

            (handler-case
                (progn 
                    (let
                        ((values (msg-slot-value (call-client-srv action param) :values)))
                        

                    (cond
                        ((eql *verbose* t)
                            (let ((error-message (concatenate 'string "Restored ontologenius/" *name* )))
                                    (print error-message)))

                        ((value values)))))

                (roslisp::ros-rpc-error () 
                    (cond
                        ((eql *verbose* t)
                            (print "Failure of service restoration"))

                        (nil))

                    )))))


(defun call-str (action param)
    """Call the service set up in the constructor of ClientBase with the request
    defined with action(str) and param(str) and returns all the first result (str).
    If the service call fails, the function returns None"""
    (setf *cpt* (+ 1 *cpt*))
        (handler-case 
            (progn 
                (setq response-cient-srv (call-client-srv action param))
                (setq values (slot-value response-client-srv 'ontologenius-msg:values))
                (cond 
                    ((> (length values) 0)
                     (return (nth 0 values)))
                    ((return ""))
        (roslisp::ros-rpc-error () (print "failed to connect"))        
        )
            )))

(defun call-nr (action param)
 """Call the service set up in the constructor of ClientBase with the
           request defined with action(str) and param(str).
           If the service call fails, the function returns False
        """
        (setf *cpt* (+ 1 *cpt*))
        (handler-case 
            (progn 
                (setq response-cient-srv (call-client-srv action param))
                (return t)
        )
        )
 ;;TODO failure handling of service 
)

(defun call-bool (action param)
 """Call the service set up in the constructor of ClientBase with the
           request defined with action(str) and param(str).
           If the service call fails, the function returns False
        """
        (setf *cpt* (+ 1 *cpt*))
        (handler-case 
            (progn 
                (setq response-cient-srv (call-client-srv action param))
                (setq code (slot-value response-client-srv 'ontologenius-msg:code))
                (setq code 0)
                (return code)
        )
        )
 ;;TODO failure handling of service 
)
