---
title: "Cryptography"
slug: "cryptography"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Find out how to encrypt and decrypt data with Go. Keep in mind that this is not a course about cryptography but rather how to achieve it with Go.

## Encryption and decryption
# Foreword
This is a detailed example about how to encrypt and decrypt data with Go. The uses code is shorten, e.g. the error handling is not mentioned. The full working project with error handling and user interface could be found on Github [here][1].

----
# Encryption

## Introduction and data
This example describes a full working encryption and decryption in Go. In order to do so, we need a data. In this example, we use our own data structure `secret`:

    type secret struct {
        DisplayName       string
        Notes             string
        Username          string
        EMail             string
        CopyMethod        string
        Password          string
        CustomField01Name string
        CustomField01Data string
        CustomField02Name string
        CustomField02Data string
        CustomField03Name string
        CustomField03Data string
        CustomField04Name string
        CustomField04Data string
        CustomField05Name string
        CustomField05Data string
        CustomField06Name string
        CustomField06Data string
    }

Next, we want to encrypt such a `secret`. The full working example could be found [here (link to Github)][1]. Now, the step-by-step process:

## Step 1
First of all, we need a kind of master password to protected the secret: `masterPassword := "PASS"`

## Step 2
All the crypto methods working with bytes instead of strings. Thus, we construct a byte array with the data from our secret.

    secretBytesDecrypted := []byte(fmt.Sprintf("%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n",
        artifact.DisplayName,
        strings.Replace(artifact.Notes, "\n", string(65000), -1),
        artifact.Username,
        artifact.EMail,
        artifact.CopyMethod,
        artifact.Password,
        artifact.CustomField01Name,
        artifact.CustomField01Data,
        artifact.CustomField02Name,
        artifact.CustomField02Data,
        artifact.CustomField03Name,
        artifact.CustomField03Data,
        artifact.CustomField04Name,
        artifact.CustomField04Data,
        artifact.CustomField05Name,
        artifact.CustomField05Data,
        artifact.CustomField06Name,
        artifact.CustomField06Data,
    ))

## Step 3
We create some salt in order to prevent rainbow table attacks, cf. [Wikipedia][2]: `saltBytes := uuid.NewV4().Bytes()`. Here, we use an UUID v4 which is not predictable.

## Step 4
Now, we are able to derive a key and a vector out of the master password and the random salt, regarding RFC 2898:

    keyLength := 256
    rfc2898Iterations := 6
    
    keyVectorData := pbkdf2.Key(masterPassword, saltBytes, rfc2898Iterations, (keyLength/8)+aes.BlockSize, sha1.New)
    keyBytes := keyVectorData[:keyLength/8]
    vectorBytes := keyVectorData[keyLength/8:]

## Step 5
The desired CBC mode works with whole blocks. Thus, we have to check if our data is aligned to a full block. If not, we have to pad it:

    if len(secretBytesDecrypted)%aes.BlockSize != 0 {
        numberNecessaryBlocks := int(math.Ceil(float64(len(secretBytesDecrypted)) / float64(aes.BlockSize)))
        enhanced := make([]byte, numberNecessaryBlocks*aes.BlockSize)
        copy(enhanced, secretBytesDecrypted)
        secretBytesDecrypted = enhanced
    }

## Step 6
Now we create an AES cipher: `aesBlockEncrypter, aesErr := aes.NewCipher(keyBytes)`

## Step 7
We reserve the necessary memory for the encrypted data: `encryptedData := make([]byte, len(secretBytesDecrypted))`. In case of AES-CBC, the encrypted data had the same length as the unencrypted data.

## Step 8
Now, we should create the encrypter and encrypt the data:

    aesEncrypter := cipher.NewCBCEncrypter(aesBlockEncrypter, vectorBytes)
    aesEncrypter.CryptBlocks(encryptedData, secretBytesDecrypted)
Now, the encrypted data is inside the `encryptedData` variable.

## Step 9
The encrypted data must be stored. But not only the data: Without the salt, the encrypted data could not be decrypted. Thus, we must use some kind of file format to manage this. Here, we encode the encrypted data as base64, cf. [Wikipedia][3]:

    encodedBytes := make([]byte, base64.StdEncoding.EncodedLen(len(encryptedData)))
    base64.StdEncoding.Encode(encodedBytes, encryptedData)

Next, we define our file content and our own file format. The format looks like this: `salt[0x10]base64 content`. First, we store the salt. In order to mark the beginning of the base64 content, we store the byte `10`. This works, because base64 does not use this value. Therefore, we could find the start of base64 by search the first occurrence of `10` from the end to the beginning of the file.

    fileContent := make([]byte, len(saltBytes))
    copy(fileContent, saltBytes)
    fileContent = append(fileContent, 10)
    fileContent = append(fileContent, encodedBytes...)

## Step 10
Finally, we could write our file: `writeErr := ioutil.WriteFile("my secret.data", fileContent, 0644)`.

----
# Decryption

## Introduction and data
As for encryption, we need some data to work with. Thus, we assume we have an encrypted file and the mentioned structure `secret`. The goal is to read the encrypted data from the file, decrypt it, and create an instance of the structure.

## Step 1
The first step is identical to the encryption: We need a kind of master password to decrypt the secret: `masterPassword := "PASS"`.

## Step 2
Now, we read the encrypted data from file: `encryptedFileData, bytesErr := ioutil.ReadFile(filename)`.

## Step 3
As mentioned before, we could split salt and encrypted data by the delimiter byte `10`, searched backwards from the end to the beginning:

    for n := len(encryptedFileData) - 1; n > 0; n-- {
        if encryptedFileData[n] == 10 {
            saltBytes = encryptedFileData[:n]
            encryptedBytesBase64 = encryptedFileData[n+1:]
            break
        }
    } 

## Step 4
Next, we must decode the base64 encoded bytes:

    decodedBytes := make([]byte, len(encryptedBytesBase64))
    countDecoded, decodedErr := base64.StdEncoding.Decode(decodedBytes, encryptedBytesBase64)
    encryptedBytes = decodedBytes[:countDecoded]

## Step 5
Now, we are able to derive a key and a vector out of the master password and the random salt, regarding RFC 2898:

    keyLength := 256
    rfc2898Iterations := 6
    
    keyVectorData := pbkdf2.Key(masterPassword, saltBytes, rfc2898Iterations, (keyLength/8)+aes.BlockSize, sha1.New)
    keyBytes := keyVectorData[:keyLength/8]
    vectorBytes := keyVectorData[keyLength/8:]

## Step 6
Create an AES cipher: `aesBlockDecrypter, aesErr := aes.NewCipher(keyBytes)`.

## Step 7
Reserve the necessary memory for the decrypted data: `decryptedData := make([]byte, len(encryptedBytes))`. By definition, it has the same length as the encrypted data.

## Step 8
Now, create the decrypter and decrypt the data:

    aesDecrypter := cipher.NewCBCDecrypter(aesBlockDecrypter, vectorBytes)
    aesDecrypter.CryptBlocks(decryptedData, encryptedBytes)

## Step 9
Convert the read bytes to string: `decryptedString := string(decryptedData)`. Because we need lines, split the string: `lines := strings.Split(decryptedString, "\n")`.

## Step 10
Construct a `secret` out of the lines:

    artifact := secret{}
    artifact.DisplayName = lines[0]
    artifact.Notes = lines[1]
    artifact.Username = lines[2]
    artifact.EMail = lines[3]
    artifact.CopyMethod = lines[4]
    artifact.Password = lines[5]
    artifact.CustomField01Name = lines[6]
    artifact.CustomField01Data = lines[7]
    artifact.CustomField02Name = lines[8]
    artifact.CustomField02Data = lines[9]
    artifact.CustomField03Name = lines[10]
    artifact.CustomField03Data = lines[11]
    artifact.CustomField04Name = lines[12]
    artifact.CustomField04Data = lines[13]
    artifact.CustomField05Name = lines[14]
    artifact.CustomField05Data = lines[15]
    artifact.CustomField06Name = lines[16]
    artifact.CustomField06Data = lines[17]

Finally, re-create the line breaks within the notes field: `artifact.Notes = strings.Replace(artifact.Notes, string(65000), "\n", -1)`.

  [1]: https://github.com/SommerEngineering/PasswordManager/blob/master/EncryptFile.go
  [2]: https://en.wikipedia.org/wiki/Salt_(cryptography)
  [3]: https://en.wikipedia.org/wiki/Base64

