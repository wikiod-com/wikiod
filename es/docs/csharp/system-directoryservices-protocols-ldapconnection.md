---
title: "System.DirectoryServices.Protocols.LdapConnection"
slug: "systemdirectoryservicesprotocolsldapconnection"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

## Conexión SSL LDAP autenticada, el certificado SSL no coincide con el DNS inverso
Configure algunas constantes para el servidor y la información de autenticación. Asumiendo LDAPv3, pero es bastante fácil cambiar eso.
  
    // Authentication, and the name of the server.
    private const string LDAPUser = "cn=example:app:mygroup:accts,ou=Applications,dc=example,dc=com";
    private readonly char[] password = { 'p', 'a', 's', 's', 'w', 'o', 'r', 'd' };
    private const string TargetServer = "ldap.example.com";

    // Specific to your company. Might start "cn=manager" instead of "ou=people", for example.
    private const string CompanyDN = "ou=people,dc=example,dc=com"; 

En realidad, cree la conexión con tres partes: un LdapDirectoryIdentifier (el servidor) y NetworkCredentials.

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

Utilice el servidor LDAP, p. busque a alguien por ID de usuario para todos los valores de clase de objeto.
El objectClass está presente para demostrar una búsqueda compuesta:
El ampersand es el operador booleano "y" para las dos cláusulas de consulta.

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

## LDAP anónimo súper simple
Asumiendo LDAPv3, pero es bastante fácil cambiar eso. Esta es una creación anónima y sin cifrar de LDAPv3 LdapConnection.

    private const string TargetServer = "ldap.example.com";

En realidad, cree la conexión con tres partes: un LdapDirectoryIdentifier (el servidor) y NetworkCredentials.

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

Para usar la conexión, algo como esto conseguiría personas con el apellido Smith
     
    SearchRequest searchRequest = new SearchRequest("dn=example,dn=com", "(sn=Smith)", SearchScope.Subtree,null);


