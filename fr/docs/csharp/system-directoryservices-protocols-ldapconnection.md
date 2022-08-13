---
title: "System.DirectoryServices.Protocols.LdapConnection"
slug: "systemdirectoryservicesprotocolsldapconnection"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

## Connexion LDAP SSL authentifiée, le certificat SSL ne correspond pas au DNS inverse
Configurez des constantes pour le serveur et les informations d'authentification. En supposant LDAPv3, mais il est assez facile de changer cela.
  
    // Authentication, and the name of the server.
    private const string LDAPUser = "cn=example:app:mygroup:accts,ou=Applications,dc=example,dc=com";
    private readonly char[] password = { 'p', 'a', 's', 's', 'w', 'o', 'r', 'd' };
    private const string TargetServer = "ldap.example.com";

    // Specific to your company. Might start "cn=manager" instead of "ou=people", for example.
    private const string CompanyDN = "ou=people,dc=example,dc=com"; 

Créez en fait la connexion en trois parties : un LdapDirectoryIdentifier (le serveur) et NetworkCredentials.

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

Utilisez le serveur LDAP, par ex. rechercher quelqu'un par ID utilisateur pour toutes les valeurs objectClass.
L'objectClass est présent pour illustrer une recherche composée :
L'esperluette est l'opérateur booléen "et" pour les deux clauses de requête.

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

## LDAP anonyme super simple
En supposant LDAPv3, mais il est assez facile de changer cela. Il s'agit d'une création LDAPv3 LdapConnection anonyme et non chiffrée.

    private const string TargetServer = "ldap.example.com";

Créez en fait la connexion en trois parties : un LdapDirectoryIdentifier (le serveur) et NetworkCredentials.

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

Pour utiliser la connexion, quelque chose comme ça amènerait les gens avec le nom de famille Smith
     
    SearchRequest searchRequest = new SearchRequest("dn=example,dn=com", "(sn=Smith)", SearchScope.Subtree,null);


