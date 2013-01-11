Semantic Networks [AI Lab]
==========================

Attribute inference in semantic network (in `Scheme`).
Based on `ISA`(is a) and `AKO`(a kind of) relations.

* `A isa B` - shows that object A belongs to the set B
* `A ako B` - shows that set A is included in set B

Running
-------

    (infer-attr 'Prius 'Attr-electric-power hybrid-net)
    
    ; (infer-attr -node-to-look-up-attr-value-for-
    ;             -attribute-whom-value-is-inferred-
    ;             -semantic-net-in-specific-representation-
    ;             )

Semantic network representation
-------------------------------

    '(
      (Constants ...) ; Constants used in representation
      
      ((ISA . 2) (AKO . 2) (Attr-sample . 2) ... (relationship predicates . arity))
      
      () ; UNUSED - legacy
      
      (  ; Statements
        (Attr-color Pufi Black)
        (ISA Pufi Dog) ; Meaning 'Pufi is a Dog'
        (AKO Dog Pet)
      )
    )
