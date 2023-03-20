(in-package :onto)

(defvar *verbose* nil)
(defvar *cpt* 0)

(defvar *client-srv* nil "ROS service to the ontology client")
(defvar *name* nil)



(defclass client-base ()
    (
        (client-name
        :initarg :client-name
        :initform (error "Must supply a service name."))
        
        client-srv
        ;;:initarg :client-srv
        ;;:initform ""
        )
    (:documentation "The ClientBase class provides an abstraction for any ROS services.
        This class ensures a persistent connection with the service based on.
        The persistent connection ensures a minimal response time.
        A reconnection logic is implemented in the event that the persistent connection fails.")
    )


(defmethod initialize-instance :after ((client client-base) &key)
    "Constructs a ROS client linked to the service name(str)."
    (let ((client-name (slot-value client 'client-name)))
      (setf *client-srv* (setf (slot-value client 'client-srv)
            (concatenate 'string "ontologenius/" client-name)
        ))
    )
    
)

;; (defun init-client (name)
;;     "Constructs a ROS client linked to the service name(str)."
;;     (setf *name* name)
;;     (setf *client-srv* (concatenate 'string "ontologenius/" *name* )))

(defun call-client-srv (action param)
    "Function to call the ontology client service."
  (roslisp:call-service *client-srv*
                'ontologenius-srv:OntologeniusService
                :action action
                :param param
                )) 

(defun nb ()
    "Gives the total number (int) of service calls from all ClientBase instances since the last reset."
    (values *cpt*))

(defun reset-nb ()
    "Reset Call Counter for all instances of ClientBase."
    (setf *cpt* 0))

(defun set-verbose (verbose)
    "If verbose(bool) is set to True, the clients will post messages about
    the failure to call the services and about their restoration."
    (setf *verbose* verbose))

(defun call (action param)
    "Call the service set up in the constructor of ClientBase with the request
    defined with action(str) and param(str) and returns all the results (str[]).
    If the service call fails, the function returns None" 

    (setf *cpt* (+ 1 *cpt*))

    (handler-case 
        (progn 
            (let
                ((response-values (roslisp:msg-slot-value (call-client-srv action param) :values)))
                (values response-values)))

        (roslisp::ros-rpc-error () 
            (cond
                ((eql *verbose* t)
                    (let ((error-message (concatenate 'string "Failure to call ontologenius/" *name* )))
                            (print error-message))))

            (setf *client-srv* (concatenate 'string "ontologenius/" *name* ))

            (handler-case
                (progn 
                    (let
                        ((response-values (roslisp:msg-slot-value (call-client-srv action param) :values)))
                        

                    (cond
                        ((eql *verbose* t)
                            (let ((error-message (concatenate 'string "Restored ontologenius/" *name* )))
                                    (print error-message))))

                    (values response-values)))

                (roslisp::ros-rpc-error () 
                    (cond
                        ((eql *verbose* t)
                            (print "Failure of service restoration")))

                        (values nil))

                    ))))

(defun call-str (action param)
    "Call the service set up in the constructor of ClientBase with the request
    defined with action(str) and param(str) and returns all the first result (str).
    If the service call fails, the function returns None"

    (setf *cpt* (+ 1 *cpt*))

    (handler-case 
        (progn 
            (let
                ((response-values (roslisp:msg-slot-value (call-client-srv action param) :values)))

                (cond 
                    ((> (length response-values) 0)
                        (values (nth 0 response-values)))

                    ((not (> (length response-values) 0) (values ""))) ;;else actually
                    )))

        (roslisp::ros-rpc-error () 
            (cond
                ((eql *verbose* t)
                    (let ((error-message (concatenate 'string "Failure to call ontologenius/" *name* )))
                            (print error-message))))

            (setf *client-srv* (concatenate 'string "ontologenius/" *name* ))

            (handler-case
                (progn 
                    (let
                        ((response-values (roslisp:msg-slot-value (call-client-srv action param) :values )))
                        

                    (cond
                        ((eql *verbose* t)
                            (let ((error-message (concatenate 'string "Restored ontologenius/" *name* )))
                                    (print error-message)))
                        
                        ((> (length response-values) 0)
                            (values (nth 0 response-values)))

                        ((not (> (length response-values) 0) (values "")))
                        )))

                (roslisp::ros-rpc-error () 
                    (cond
                        ((eql *verbose* t)
                            (print "Failure of service restoration")))

                    (values nil))

                    ))))


(defun call-nr (action param)
   "Call the service set up in the constructor of ClientBase with the
    request defined with action(str) and param(str).
    If the service call fails, the function returns False"

    (setf *cpt* (+ 1 *cpt*))

        (handler-case 
            (progn 
                (let
                    ((response (call-client-srv action param)))
                    (values t)))

            (roslisp::ros-rpc-error () 
                (cond
                    ((eql *verbose* t)
                        (let ((error-message (concatenate 'string "Failure to call ontologenius/" *name* )))
                                (print error-message))))

                    (setf *client-srv* (concatenate 'string "ontologenius/" *name* ))

                (handler-case
                    (progn 
                        (let
                            ((response (call-client-srv action param)))
                        (cond
                            ((eql *verbose* t)
                                (let ((error-message (concatenate 'string "Restored ontologenius/" *name* )))
                                        (print error-message))))

                            (values t)))

                    (roslisp::ros-rpc-error () 
                        (cond
                            ((eql *verbose* t)
                                (print "Failure of service restoration")))

                            (values nil))

                        ))))

(defun call-bool (action param)
    "Call the service set up in the constructor of ClientBase with the
    request defined with action(str) and param(str).
    Returns False if the service call fails or the result code of the
    service is different from SUCCESS."

    (setf *cpt* (+ 1 *cpt*))

    (handler-case 
        (progn 
            (let
                ((response (call-client-srv action param)))
                (let ((response-code (roslisp:msg-slot-value response :code)))
                
                (eql response-code 0))))

        (roslisp::ros-rpc-error () 
            (cond
                ((eql *verbose* t)
                    (let ((error-message (concatenate 'string "Failure to call ontologenius/" *name* )))
                            (print error-message)))

                ((setf *client-srv* (concatenate 'string "ontologenius/" *name* ))))

            (handler-case
                (progn 
                    (let
                        ((response-code (roslisp:msg-slot-value (call-client-srv action param) :code)))
                        

                    (cond
                        ((eql *verbose* t)
                            (let ((error-message (concatenate 'string "Restored ontologenius/" *name* )))
                                    (print error-message))))

                        (eql response-code 0)))

                (roslisp::ros-rpc-error () 
                    (cond
                        ((eql *verbose* t)
                            (print "Failure of service restoration")))

                        (values nil)

                    )))))
