---
title: "CAPTCHA Helper"
slug: "captcha-helper"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Complete example
Here is an example of usage with a database. On the page where the CAPTCHA will be shown you'll have something like this:

    $this->load->helper('captcha');
    $vals = array(
        'img_path'    => './captcha/',
        'img_url'    => 'http://example.com/captcha/'
        );
    
    $cap = create_captcha($vals);
    
    $data = array(
        'captcha_time'    => $cap['time'],
        'ip_address'    => $this->input->ip_address(),
        'word'    => $cap['word']
        );
    
    $query = $this->db->insert_string('captcha', $data);
    $this->db->query($query);
    
    echo 'Submit the word you see below:';
    echo $cap['image'];
    echo '<input type="text" name="captcha" value="" />';

Then, on the page that accepts the submission you'll have something like this:

    // First, delete old captchas
    $expiration = time()-7200; // Two hour limit
    $this->db->query("DELETE FROM captcha WHERE captcha_time < ".$expiration);  

    // Then see if a captcha exists:
    $sql = "SELECT COUNT(*) AS count FROM captcha WHERE word = ? AND ip_address = ? AND captcha_time > ?";
    $binds = array($_POST['captcha'], $this->input->ip_address(), $expiration);
    $query = $this->db->query($sql, $binds);
    $row = $query->row();
    
    if ($row->count == 0)
    {
        echo "You must submit the word that appears in the image";
    }

## Loading this Helper
This helper is loaded using the following code:

**In Controller itself**(* can repeat again and again*)

    $this->load->helper('captcha');

**In `config/autoload.php`** (*Load only once*)

    $autoload['helper'] = array('captcha');

## create_captcha($data)
Takes an array of information to generate the CAPTCHA as input and creates the image to your specifications, returning an array of associative data about the image.

    [array]
    (
      'image' => IMAGE TAG
      'time'    => TIMESTAMP (in microtime)
      'word'    => CAPTCHA WORD
    )

The "image" is the actual image tag:

    <img src="http://example.com/captcha/12345.jpg" width="140" height="50" />
The "time" is the micro timestamp used as the image name without the file extension. It will be a number like this: `1139612155.3422`

The "word" is the word that appears in the captcha image, which if not supplied to the function, will be a random string.

## Using the CAPTCHA helper
Once loaded you can generate a captcha like this:

    $vals = array(
        'word'    => 'Random word',
        'img_path'    => './captcha/',
        'img_url'    => 'http://example.com/captcha/',
        'font_path'    => './path/to/fonts/texb.ttf',
        'img_width'    => '150',
        'img_height' => 30,
        'expiration' => 7200
        );
    
    $cap = create_captcha($vals);
    echo $cap['image'];

 - The captcha function requires the GD image library.
 - Only the `img_path` and `img_url` are required.
 - If a "word" is not supplied, the function will generate a random ASCII string. You might put together your own word library that you can draw randomly from.
 - If you do not specify a path to a TRUE TYPE font, the native ugly GD font will be used. The "captcha" folder must be writable (666, or 777)
 - The "expiration" (in seconds) signifies how long an image will remain in the captcha folder before it will be deleted. The default is two hours.



