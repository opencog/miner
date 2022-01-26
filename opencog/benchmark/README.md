
                  OpenCog Miner Benchmark
                  -----------------------

The script for the benchmark is found https://github.com/singnet/bioatomspace_reasoning/blob/master/mine-bio-as.scm
The purpose of this benchmark is to measure the performance of the type and glob
support in the pattern miner.

**Dataset Sample**

*smpdb_gene_2020_07_22.scm*

    (MemberLink
        (GeneNode "GPD1")
        (SmpNode "SMP0038938"))
    ...

*GO_annotation_gene-level_2020-07-21.scm*

    (MemberLink
        (GeneNode "NOVA2")
        (MolecularFunctionNode "GO:0003729"))
    ...

*GO_2020_07_22.scm*

    (EvaluationLink
        (PredicateNode "has_name")
        (ListLink
            (BiologicalProcessNode "GO:0000001")
            (ConceptNode "mitochondrion inheritance")))
    (InheritanceLink
        (BiologicalProcessNode "GO:0000001")
        (BiologicalProcessNode "GO:0048308"))
    (InheritanceLink
        (BiologicalProcessNode "GO:0000001")
        (BiologicalProcessNode "GO:0048311"))

There are Four runs of the pattern miner to compare.

**Control**

This is a run where the codes for the type checking and glob support are not
included.
The initial pattern for the miner is

    (Lambda
        (VariableSet
            (Variable "$Gene")
            (Variable "$SMP")
            (Variable "$GO"))
        (Present
            (Member (Variable "$Gene") (Variable "$SMP"))
            (Member (Variable "$Gene") (Variable "$GO"))
            (Inheritance (Variable "$SMP") (Concept "SMP_term"))
            (Inheritance (Variable "$GO") (Concept "GO_term"))))

In order to specialize on the above pattern we need an additional data containing
the inheritance.
The `(add-extra-smp-go-terms)` in `mine-bio-as.scm` does exactly that.

`control.dat` contains the profile for this run. It took 10547 sec and
6038(6785 virtual RAM) MB of memory on average to run.

**Type and Glob disabled**

This is a run where the code for the type check and glob support are included
but both type check and glob support are disabled in the miner.

    ...
    #:enable-type #f
    #:enable-glob #f)

The initial pattern and the extra data is the same as for control.
The profile is found in `type_glob_disabled.dat`. It took approximately the
same time and memory as control.

**Type enabled**

This is where the enable-type is enabled and glob support is disabled.
For this run we change the initial pattern to:-

    (Lambda
        (VariableSet
            (TypedVariable (Variable "$Gene") (Type "GeneNode"))
            (TypedVariable (Variable "$SMP") (Type "SmpNode"))
            (TypedVariable (Variable "$GO") (TypeInhNode "OntologyNode")))
        (Present
            (Member (Variable "$Gene") (Variable "$SMP"))
            (Member (Variable "$Gene") (Variable "$GO"))))

and the additional data `(add-extra-smp-go-terms)` is removed.

`type_enabled.scm` contains the profile of this run. It took 3307 sec and 4979 MB
of memory on average to run.

**Type Glob enabled**

Both type and glob support are enabled, the dataset and the initial pattern
are kept the same as for type enabled above.

Due to the dataset, glob support couldn't find any specializations on the initial
pattern. So the glob support turned out to be trivial for this particular case
and we need to run the miner on a different dataset to see the performance of the
glob support.
The time and memory it took to run are the same as for the type enabled run above.

**Conclusion**

- The type support was 3x faster and took 20% less memory for the gene level
dataset in the bio-atomspace compared to adding type ontology as inheritance.

- Even though the performance of enable-glob is unknown, from
`Type and Glob disabled` and `Type Glob enabled` we know that the glob support
code is not malicious to the rest of the miner.


Author Kasim<se.kasim.ebrahim@gmail.com>
