---
title: "System.DirectoryServices.Protocols.LdapConnection"
slug: "systemdirectoryservicesprotocolsldapconnection"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

## Conexão SSL LDAP autenticada, certificado SSL não corresponde ao DNS reverso
Configure algumas constantes para o servidor e informações de autenticação. Assumindo LDAPv3, mas é fácil mudar isso.
  
    // Authentication, and the name of the server.
    private const string LDAPUser = "cn=example:app:mygroup:accts,ou=Applications,dc=example,dc=com";
    private readonly char[] password = { 'p', 'a', 's', 's', 'w', 'o', 'r', 'd' };
    private const string TargetServer = "ldap.example.com";

    // Specific to your company. Might start "cn=manager" instead of "ou=people", for example.
    private const string CompanyDN = "ou=people,dc=example,dc=com"; 

Na verdade, crie a conexão com três partes: um LdapDirectoryIdentifier (o servidor) e NetworkCredentials.

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

Use o servidor LDAP, por exemplo. procure alguém por userid para todos os valores de objectClass.
A objectClass está presente para demonstrar uma pesquisa composta:
O e comercial é o operador booleano "and" para as duas cláusulas de consulta.

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

## LDAP anônimo super simples
Assumindo LDAPv3, mas é fácil mudar isso. Esta é a criação anônima e não criptografada do LDAPv3 LdapConnection.

    private const string TargetServer = "ldap.example.com";

Na verdade, crie a conexão com três partes: um LdapDirectoryIdentifier (o servidor) e NetworkCredentials.

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

Para usar a conexão, algo assim faria com que as pessoas com o sobrenome Smith
     
    SearchRequest searchRequest = new SearchRequest("dn=example,dn=com", "(sn=Smith)", SearchScope.Subtree,null);


