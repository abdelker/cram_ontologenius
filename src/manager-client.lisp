(in-package :onto)

(defun init-manager ()
"""Constructs a manager client.
           Can only be used in a multi-ontology mode.
        """
(init-client "manage")

)

(defun list ()
"""Returns the name of the instantiated ontologies (str[])."""
    (call "list" "")
)

(defun add (name)
"""Create an ontology instance named name(str).s
           Returns False if the service call fails.
        """
    (call-nr "add" name)
)

(defun copy (dest-name src-name)
"""Create a copy of the ontology instance named src_name(str) with the name dest_name(str).
           Returns False if the service call fails or if the copy fails.
        """
    (setf name (concatenate 'string dest-name "=" src-name)   
    (call-bool "add" name)
))

(defun delete (name)
"""Delete the instance of the ontology named name(str)
           Returns False if the service call fails.
        """
    (call-nr "delete" name)
)

(defun get-difference (onto1 onto2 concept)
"""Returns the difference of knowledge between onto_1(str) and onto_2(str) regarding the concept (class or individual) concept(str).
           The elements of the returned vector are formated as : [+]concept_from|property|concept_on OR [-]concept_from|property|concept_on.
           An element is positive if it is present in onto_1 and not in onto_2 and negative in reverse.
           The difference in inheritance knowledge between concepts is returned with the property isA.
        """
    (setf onto (concatenate 'string onto1 "|" onto2 "|" concept) 
    (call-nr "difference" onto)
))