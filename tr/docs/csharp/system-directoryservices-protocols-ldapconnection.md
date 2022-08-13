---
title: "System.DirectoryServices.Protocols.LdapConnection"
slug: "systemdirectoryservicesprotocolsldapconnection"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

## Kimliği doğrulanmış SSL LDAP bağlantısı, SSL sertifikası ters DNS ile eşleşmiyor
Sunucu ve kimlik doğrulama bilgileri için bazı sabitler ayarlayın. LDAPv3'ü varsayarsak, ancak bunu değiştirmek yeterince kolaydır.
  
    // Authentication, and the name of the server.
    private const string LDAPUser = "cn=example:app:mygroup:accts,ou=Applications,dc=example,dc=com";
    private readonly char[] password = { 'p', 'a', 's', 's', 'w', 'o', 'r', 'd' };
    private const string TargetServer = "ldap.example.com";

    // Specific to your company. Might start "cn=manager" instead of "ou=people", for example.
    private const string CompanyDN = "ou=people,dc=example,dc=com"; 

Aslında bağlantıyı üç bölümden oluşturun: bir LdapDirectoryIdentifier (sunucu) ve NetworkCredentials.

    // Configure server and port. LDAP w/ SSL, aka LDAPS, uses port 636.
    // If you don't have SSL, don't give it the SSL port. 
    LdapDirectoryIdentifier identifier = new LdapDirectoryIdentifier(TargetServer, 636);

    // Configure network credentials (userid and password)
    var secureString = new SecureString();
    foreach (var character in password)
            secureString.AppendChar(character);
    NetworkCredential creds = new NetworkCredential(LDAPUser, secureString);
    
    // Actually create the connection
    LdapConnection connection = new LdapConnection(identifier, creds)
    {
        AuthType = AuthType.Basic, 
        SessionOptions =
        {
            ProtocolVersion = 3,
            SecureSocketLayer = true
        }
    };

    // Override SChannel reverse DNS lookup.
    // This gets us past the "The LDAP server is unavailable." exception
    // Could be 
    //    connection.SessionOptions.VerifyServerCertificate += { return true; };
    // but some certificate validation is probably good.
    connection.SessionOptions.VerifyServerCertificate +=
        (sender, certificate) => certificate.Subject.Contains(string.Format("CN={0},", TargetServer));

LDAP sunucusunu kullanın, örn. tüm objectClass değerleri için kullanıcı kimliğine göre birini arayın.
ObjectClass, bir bileşik aramayı göstermek için mevcuttur:
Ve işareti, iki sorgu yan tümcesi için boolean "ve" operatörüdür.

     SearchRequest searchRequest = new SearchRequest(
            CompanyDN, 
            string.Format((&(objectClass=*)(uid={0})), uid), 
            SearchScope.Subtree,
            null
    );

    // Look at your results
    foreach (SearchResultEntry entry in searchResponse.Entries) {
        // do something
    }

## Süper Basit anonim LDAP
LDAPv3'ü varsayarsak, ancak bunu değiştirmek yeterince kolaydır. Bu, anonim, şifrelenmemiş LDAPv3 LdapConnection oluşturma işlemidir.

    private const string TargetServer = "ldap.example.com";

Aslında bağlantıyı üç bölümden oluşturun: bir LdapDirectoryIdentifier (sunucu) ve NetworkCredentials.

    // Configure server and credentials
    LdapDirectoryIdentifier identifier = new LdapDirectoryIdentifier(TargetServer);
    NetworkCredential creds = new NetworkCredential();
    LdapConnection connection = new LdapConnection(identifier, creds)   
    {
        AuthType=AuthType.Anonymous,
        SessionOptions =
        {
            ProtocolVersion = 3
        }
    };

Bağlantıyı kullanmak için, bunun gibi bir şey, Smith soyadına sahip insanları alır.
     
    SearchRequest searchRequest = new SearchRequest("dn=example,dn=com", "(sn=Smith)", SearchScope.Subtree,null);


