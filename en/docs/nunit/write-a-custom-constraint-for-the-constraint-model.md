---
title: "Write a custom constraint for the constraint model"
slug: "write-a-custom-constraint-for-the-constraint-model"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Match an integer approximatively
Suppose we want to write a constraint which matches a number, but approximatively. Say, you are supposed to have `95` people in a survey, but `93` or `96` will do as well. We can write a custom constraint of the form:

<!-- language: csharp -->
    public class AlmostEqualToConstraint : Constraint
    {
        readonly int _expected;
        readonly double _expectedMin;
        readonly double _expectedMax;
        readonly int _percentageTolerance;


        public AlmostEqualToConstraint(int expected, int percentageTolerance)
        {
            _expected = expected;
            _expectedMin = expected * (1 - (double)percentageTolerance / 100);
            _expectedMax = expected * (1 + (double)percentageTolerance / 100);                    
            _percentageTolerance = percentageTolerance;
            Description = $"AlmostEqualTo {expected} with a tolerance of {percentageTolerance}%";
        }


        public override ConstraintResult ApplyTo<TActual>(TActual actual)
        {
            if (typeof(TActual) != typeof(int))
                return new ConstraintResult(this, actual, ConstraintStatus.Error);

            var actualInt = Convert.ToInt32(actual);

            if (_expectedMin <= actualInt && actualInt <= _expectedMax)
                return new ConstraintResult(this, actual, ConstraintStatus.Success);
            else
                return new ConstraintResult(this, actual, ConstraintStatus.Failure);
        }
    }


## Make new constraint usable in a fluent context
We're going to integrate the `AlmostEqualToConstraint` with the fluent NUnit interfaces, specifically the `Is` one. We'll need to extend the NUnit provided `Is` and use that throughout our code.

<!-- language: csharp -->

    public class Is : NUnit.Framework.Is
    {
        public static AlmostEqualToConstraint AlmostEqualTo(int expected, int percentageTolerance = 5)
        {
            return new AlmostEqualToConstraint(expected, percentageTolerance);
        }
    }

