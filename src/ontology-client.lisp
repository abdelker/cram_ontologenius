(in-package :onto)

(defun init-onto (name)
    """Constructs an ontology client linked to the service ontologenius/name(str)."""
    (init-client name)
)

(defun get-up (name &optional (depth -1) (selector ""))
    """Gives all concepts below the one given in the parameter: name(str).
    The optional depth(int) parameter can be set to limit tree propagation to a specific value.
    The default value -1 represents no propagation limitation.
    The optional selector(str) parameter can be set to only get results inheriting from the selector(str) concept.
    The default value '' represents no restriction on the result."""
    (let ((param name))
        (cond 
            ((not (string= selector ""))
                (setq param (concatenate 'string param " -s " selector)))
            
            ((>= depth 0)
                (setq param (concatenate 'string param " -d " 
                ;;(format nil "~a" depth)
                ))))

            (call "getUp" param)
            ))

(defun is-A (name base-class)
    """Return true if the concept name(str) is or inherits of the concept base_class(str).
        This function corresponds to checking if class_base is part of the result of the function getUp applies to the concept name."""
    (let ((res (get-up  name -1 base-class)))
        (cond 
            ((eq (length res) 0)
                NIL )
            
            (t))
    ))

(defun get-name (name &optional (take-id t))
    """Gives one of the label (str) of the concept name(str) that is not muted.
        The default take_id(bool) argument can be set to False if you do not want to
        consider the concept identifier as a possible default name.
        The result of this function depends on the setting of the working language."""
    (let ((param name))
        (cond 
            ((eql take-id nil)
                (setq param (concatenate 'string param " -i false"))))

            (call-str "getName" param)
            ))