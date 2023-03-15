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
    the failure to call the services and about their restoration."""
    (setf *verbose* verbose))

(defun call (action param)
    """Call the service set up in the constructor of ClientBase with the request
    defined with action(str) and param(str) and returns all the results (str[]).
    If the service call fails, the function returns None"""

    (setf *cpt* (+ 1 *cpt*))

    (handler-case 
        (progn 
            (let
                ((response-values (msg-slot-value (call-client-srv action param) :values)))
                (value response-values)))

        (roslisp::ros-rpc-error () 
            (cond
                ((eql *verbose* t)
                    (let ((error-message (concatenate 'string "Failure to call ontologenius/" *name* )))
                            (print error-message)))

                ((setf *client-srv* (concatenate 'string "ontologenius/" *name* ))))

            (handler-case
                (progn 
                    (let
                        ((response-values (msg-slot-value (call-client-srv action param) :values)))
                        

                    (cond
                        ((eql *verbose* t)
                            (let ((error-message (concatenate 'string "Restored ontologenius/" *name* )))
                                    (print error-message)))

                        ((value response-values)))))

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
            (let
                ((response-values (msg-slot-value (call-client-srv action param) :values)))
                (cond 
                    ((> (length response-values) 0)
                        (return (nth 0 response-values)))

                    (retrun ""))))

        (roslisp::ros-rpc-error () 
            (cond
                ((eql *verbose* t)
                    (let ((error-message (concatenate 'string "Failure to call ontologenius/" *name* )))
                            (print error-message)))

                ((setf *client-srv* (concatenate 'string "ontologenius/" *name* ))))

            (handler-case
                (progn 
                    (let
                        ((response-values (msg-slot-value (call-client-srv action param) :values)))
                        

                    (cond
                        ((eql *verbose* t)
                            (let ((error-message (concatenate 'string "Restored ontologenius/" *name* )))
                                    (print error-message)))
                        
                        ((> (length response-values) 0)
                            (return (nth 0 response-values)))

                        (retrun ""))))

                (roslisp::ros-rpc-error () 
                    (cond
                        ((eql *verbose* t)
                            (print "Failure of service restoration"))

                        (nil))

                    )))))


(defun call-nr (action param)
    """Call the service set up in the constructor of ClientBase with the
    request defined with action(str) and param(str).
    If the service call fails, the function returns False"""

    (setf *cpt* (+ 1 *cpt*))

        (handler-case 
            (progn 
                (let
                    ((response (call-client-srv action param)))
                    (t)))

            (roslisp::ros-rpc-error () 
                (cond
                    ((eql *verbose* t)
                        (let ((error-message (concatenate 'string "Failure to call ontologenius/" *name* )))
                                (print error-message)))

                    ((setf *client-srv* (concatenate 'string "ontologenius/" *name* ))))

                (handler-case
                    (progn 
                        (let
                            ((response (call-client-srv action param)))
                            

                        (cond
                            ((eql *verbose* t)
                                (let ((error-message (concatenate 'string "Restored ontologenius/" *name* )))
                                        (print error-message)))

                            (t))))

                    (roslisp::ros-rpc-error () 
                        (cond
                            ((eql *verbose* t)
                                (print "Failure of service restoration"))

                            (nil))

                        )))))

(defun call-bool (action param)
    """Call the service set up in the constructor of ClientBase with the
    request defined with action(str) and param(str).
    Returns False if the service call fails or the result code of the
    service is different from SUCCESS."""

    (setf *cpt* (+ 1 *cpt*))

    (handler-case 
        (progn 
            (let
                ((response-code (msg-slot-value (call-client-srv action param) :code)))
                ((response-code 0))))

        (roslisp::ros-rpc-error () 
            (cond
                ((eql *verbose* t)
                    (let ((error-message (concatenate 'string "Failure to call ontologenius/" *name* )))
                            (print error-message)))

                ((setf *client-srv* (concatenate 'string "ontologenius/" *name* ))))

            (handler-case
                (progn 
                    (let
                        ((response-code (msg-slot-value (call-client-srv action param) :code)))
                        

                    (cond
                        ((eql *verbose* t)
                            (let ((error-message (concatenate 'string "Restored ontologenius/" *name* )))
                                    (print error-message)))

                        ((response-code 0)))))

                (roslisp::ros-rpc-error () 
                    (cond
                        ((eql *verbose* t)
                            (print "Failure of service restoration"))

                        (nil))

                    )))))
