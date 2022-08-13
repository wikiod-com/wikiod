---
title: "Mockito Best Practices"
slug: "mockito-best-practices"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## BDDMockito style
Behavior Driven Development (BDD) testing style revolves around "given",  "when" and "then" stages in tests. However, classical Mockito uses "when" word for "given" phase, and does not include other natural language constructions that can encompass BDD. Thus, [BDDMockito][1] aliases were introduced in version 1.8.0 in order to facilitate behavior driven tests.

The most common situation is to stub returns of a method. In the following example, `getStudent(String)` method of the mocked `StudentRepository` will return `new Student(givenName, givenScore)` if invoked with an argument that is equal to `givenName`.

    import static org.mockito.BDDMockito.*;

    public class ScoreServiceTest {

        private StudentRepository studentRepository = mock(StudentRepository.class);

        private ScoreService objectUnderTest = new ScoreService(studentRepository);

        @Test
        public void shouldCalculateAndReturnScore() throws Exception {
            //given
            String givenName = "Johnny";
            int givenScore = 10;
            given(studentRepository.getStudent(givenName))
                .willReturn(new Student(givenName, givenScore));
    
            //when
            String actualScore = objectUnderTest.calculateStudentScore(givenName);
    
            //then
            assertEquals(givenScore, actualScore);
        }
    }

Sometimes it is desired to check if exception thrown from dependency is correctly handled or rethrown in a method under test. Such behavior can be stubbed in "given" phase in this way:

    willThrow(new RuntimeException())).given(mock).getData();

Sometimes it is desired to set up some side effects that a stubbed method should introduce. Especially it can come in handy when: 

 - the stubbed method is a method that is supposed to change the
   internal state of a passed object

 - the stubbed method is a void method

Such behavior can be stubbed in "given" phase with an "Answer":

    willAnswer(invocation -> this.prepareData(invocation.getArguments()[0])).given(mock).processData();

When it is desired to verify interactions with a mock, it can be done in "then" phase with `should()` or `should(VerificationMode)`(only since 1.10.5) methods:

    then(mock).should().getData(); // verifies that getData() was called once
    then(mock).should(times(2)).processData(); // verifies that processData() was called twice

When it is desired to verify that there were no more interactions with a mock besides already verified, it can be done in "then" phase with `shouldHaveNoMoreInteractions()` (since 2.0.0):

    then(mock).shouldHaveNoMoreInteractions(); // analogue of verifyNoMoreInteractions(mock) in classical Mockito

When it is desired to verify that there were absolutely no interactions with a mock, it can be done in "then" phase with `shouldHaveNoMoreInteractions()` (since 2.0.0):

    then(mock).shouldHaveZeroInteractions(); // analogue of verifyZeroInteractions(mock) in classical Mockito

When it is desired to check if [methods were invoked in order][2] it can be done in "then" phase with `should(InOrder)` (since 1.10.5) and `should(InOrder, VerificationMode)` (since 2.0.0):

    InOrder inOrder = inOrder(mock);

    // test body here

    then(mock).should(inOrder).getData(); // the first invocation on the mock should be getData() invocation
    then(mock).should(inOrder, times(2)).processData(); // the second and third invocations on the mock should be processData() invocations


  [1]: http://site.mockito.org/mockito/docs/current/org/mockito/BDDMockito.html
  [2]: http://site.mockito.org/mockito/docs/current/org/mockito/Mockito.html#in_order_verification

