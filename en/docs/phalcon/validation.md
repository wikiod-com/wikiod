---
title: "Validation"
slug: "validation"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

- API reference to the validation class can be found here: https://docs.phalconphp.com/en/latest/api/Phalcon_Validation.html
- If there is entity provided in `\Phalcon\Validation` you don't need to pass model key in `\Phalcon\Validation\Validator\Uniqueness`

## Built in Validators
**PresenceOf** - Validates that a value is not null or empty string

    $validator->add('name', new \Phalcon\Validation\Validator\PresenceOf([
       'message' => 'The name is required'
    ]));

**Email** - Checks if a value has a correct e-mail format

    $validator->add('email', new \Phalcon\Validation\Validator\Email([
       'message' => 'The e-mail is not valid'
    ]));

**Identical** - Checks if a value is identical to other

    $validator->add('terms', new \Phalcon\Validation\Validator\Identical([
       'accepted' => 'yes',
       'message' => 'Terms and conditions must be accepted'
    ]));

**Url** - Checks if a value has a url format

    $validator->add('url', new \Phalcon\Validation\Validator\Url([
       'message' => ':field must be a url'
    ]));

**Confirmation** - Checks that two values have the same value

    $validator->add('password', new \Phalcon\Validation\Validator\Confirmation([
       'message' => 'Password doesn\'t match confirmation',
       'with' => 'confirmPassword'
    ]));

**StringLength** - Validates that a string has the specified maximum and minimum constraints The test is passed if for a string’s length L, min<=L<=max, i.e. L must be at least min, and at most max.

    $validation->add('name_last', new \Phalcon\Validation\Validator\StringLength([
          'max' => 50,
          'min' => 2,
          'messageMaximum' => 'We don\'t like really long names',
          'messageMinimum' => 'We want more than just their initials'
    ]));

**Regex** - Allows validate if the value of a field matches a regular expression

    $validator->add('created_at', new \Phalcon\Validation\Validator\Regex([
       'pattern' => '/^[0-9]{4}[-\/](0[1-9]|1[12])[-\/](0[1-9]|[12][0-9]|3[01])$/',
       'message' => 'The creation date is invalid'
    ]));

**CreditCard** - Checks if a value has a valid creditcard number

    $validator->add('creditcard', new \Phalcon\Validation\Validator\CreditCard([
       'message' => 'The credit card number is not valid'
    ]));

**Between** - Validates that a value is between an inclusive range of two values. For a value x, the test is passed if minimum<=x<=maximum.

    $validator->add('name', new \Phalcon\Validation\Validator\Between([
       'minimum' => 0,
       'maximum' => 100,
       'message' => 'The price must be between 0 and 100'
    ]));

**ExclusionIn** - Check if a value is not included into a list of values

    $validator->add('status', new \Phalcon\Validation\Validator\ExclusionIn([
       'message' => 'The status must not be A or B',
       'domain' => ['A', 'B']
    ]));

**InclusionIn** - Check if a value is included into a list of values

    $validator->add('status', new \Phalcon\Validation\Validator\InclusionIn([
       'message' => 'The status must be A or B',
       'domain' => ['A', 'B']
    ]));

**Uniqueness** - Check if a value is uniqueness

    $validator->add('login', new \Phalcon\Validation\Validator\Uniqueness([
        'message' => 'The login must be unique',
        'model' => new Users()
    ]));

## Google reCaptcha custom validation component
**The class**

    use Phalcon\Validation\Validator;
    use Phalcon\Validation\ValidatorInterface;
    use Phalcon\Validation\Message;
    
    class RecaptchaValidator extends Validator implements ValidatorInterface
    {
        public function validate(\Phalcon\Validation $validation, $attribute)
        {
            $value = $validation->getValue('g-recaptcha-response');
            $ip = $validation->request->getClientAddress();
            if (!$this->verify($value, $ip)) {
                $validation->appendMessage(new Message($this->getOption('message'), $attribute, 'Recaptcha'));
                return false;
            }
            return true;
        }
        
        protected function verify($value, $ip)
        {
            $params = [
                'secret' => 'YOUR_RECAPTCHA_SECRET_KEY',
                'response' => $value,
                'remoteip' => $ip
            ];
            $response = json_decode(file_get_contents('https://www.google.com/recaptcha/api/siteverify?' . http_build_query($params)));
            return (bool) $response->success;
        }
    }

**Example usage in a Phalcon form:**

    $reCaptchaField->addValidator(new \RecaptchaValidator([
        'message' => 'Your reCaptcha error message'
    ]));

