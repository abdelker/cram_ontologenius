(in-package :onto)

(defvar *feed-pub* nil "ROS publisher")
(defvar *feed-pub-stamped* nil "ROS publisher")
(defvar *commit-sub* nil "ROS subscriber")

(defvar *pub-topic-name* "ontologenius/insert" "ROS topic name")
(defvar *sub-topic-name* "ontologenius/end" "ROS topic name to subscribe to")

(defvar *commit-cb* (make-fluent :name :commit-cb))

(defun init-feed-pub (name)
 """Constructs a FeederPublisher.
           Can be used in a multi-ontology mode by specifying the name of the ontology name(str).
           For classic use, name(str) should be defined as ''.
        """
    (cond 
        ((not(string= question ""))
            (progn 
                (setf *pub-topic-name* (concatenate 'string  *pub-topic-name* "/" name ))
                (setf *feed-pub* (advertise *pub-topic-name* "std_msgs/String"))
                (setf *pub-topic-name* "ontologenius/insert_stamped")))
        
        ((not(string= question ""))
            (progn 
                (setf *pub-topic-name* (concatenate 'string  *pub-topic-name* "/" name ))
                (setf *feed-pub-stamped*  (advertise *pub-topic-name* "ontologenius/StampedString"))
                (setf *pub-topic-name* "ontologenius/insert_stamped")))
        
        ((not(string= question ""))
            (progn
                (setf *sub-topic-name* (concatenate 'string  *pub-topic-name* "/" name )))))

    ((setf *commit-sub* (subscribe *sub-topic-name* "std_msgs/String"
                                      #'commit-callback)))))

  (defun commit-callback (msg)
    "Callback for commit values. Called by the ontologenius/end topic subscriber."
    (setf (value *commit-cb*) msg))