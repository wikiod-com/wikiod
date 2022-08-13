---
title: "How to Create Custom Matchers?"
slug: "how-to-create-custom-matchers"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Example of Custom matcher for testing TextView error message
 1. Create a class name `ErrorMatcher` inside your test package with below code:

  

    public class ErrorMatcher {

        @NonNull
        public static Matcher<View> withError(final String expectedErrorText) {
            Checks.checkNotNull(expectedErrorText);
            return new BoundedMatcher<View, TextView>(TextView.class) {    
                @Override
                public void describeTo(final Description description) {
                    description.appendText("error text: ");
                    stringMatcher.describeTo(description);
                }
                    
                @Override
                public boolean matchesSafely(final TextView textView) {
                    return expectedErrorText.equals(textView.getError().toString());
                }
            };
        }
    }

Matching logic is to find the `TextView` element, which error message text is equal to  expected error text value, going through the subset of `TextView` fields present in the layout hierarchy. `describeTo` method is used for debug output.
2. Then you can use your custom matcher in the test case as shown below:


    @Test  
    public void verifiesSignInErrorIsShown() {
        onView(withId(R.id.email_sign_in_button)).perform(click());
        onView(ErrorMatcher.withError("Your error text")).check(matches(isDisplayed()));
    }



