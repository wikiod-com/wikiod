---
title: "Configuring a JNDI datasource"
slug: "configuring-a-jndi-datasource"
draft: false
images: []
weight: 9866
type: docs
toc: true
---

## Parameters
| Attribute | Details |
| ------ | ------ |
| auth | (String) Specify whether the web Application code signs on to the corresponding resource manager programmatically, or whether the Container will sign on to the resource manager on behalf of the application. The value of this attribute must be Application or Container. This attribute is required if the web application will use a resource-ref element in the web application deployment descriptor, but is optional if the application uses a resource-env-ref instead. |
| driverClassName| (String) The fully qualified Java class name of the JDBC driver to be used. The driver has to be accessible from the same classloader as the database connection pool jar. |
| factory | (String) Full class path to the connection datasource factory. |
| initialSize | (int)The initial number of connections that are created when the pool is started. Default value is 10 |
| maxIdle | (int) The minimum number of established connections that should be kept in the pool at all times. The connection pool can shrink below this number if validation queries fail. Default value is derived from initialSize of 10 |
| maxTotal / maxActive | (int) The maximum number of active connections that can be allocated from this pool at the same time. The default value is 100. Note that this attribute name differs between pool implementations and documentation is often incorrect. |
| maxWaitMillis / maxWait | (int) The maximum number of milliseconds that the pool will wait (when there are no available connections) for a connection to be returned before throwing an exception. Default value is 30000 (30 seconds).  Note that this attribute name differs between pool implementations and documentation is often incorrect. |
| name | (String) Name used to bind to JNDI context. |
| password | (String) DB connection password. |
| url | (String) (String) JDBC connection URL. |
| username | (String) DB connection username. |
| testOnBorrow | (boolean) The indication of whether objects will be validated before being borrowed from the pool. If the object fails to validate, it will be dropped from the pool, and we will attempt to borrow another. NOTE - for a true value to have any effect, the validationQuery or validatorClassName parameter must be set to a non-null string. In order to have a more efficient validation, see validationInterval. Default value is false. |
| validationQuery | (String) The SQL query that will be used to validate connections from this pool before returning them to the caller. If specified, this query does not have to return any data, it just can't throw a SQLException. The default value is null. Example values are SELECT 1(mysql), select 1 from dual(oracle), SELECT 1(MS Sql Server) |


Attributes
---------
The list of available attributes is extensive and fully covered in [Tomcat's JDBC Connection Pool][3] reference documentation. Only the attributes used in the examples above are covered in the parameters section here.

DBCP vs Tomcat JDBC Connection Pool
---------
Many locations in reference documentation refer to use of DBCP connection pools. The history on which connection pool implementation is actually being used in Tomcat, by default, is complex and confusing. It depends on specific version of Tomcat being used. It's best to specify the factory explicitly.

Reference Documentation
---------
 - [Tomcat 8 JDNI Resources HOW-TO - JDBC Data Sources][1]
 - [Tomcat 8 JNDI Datasource HOW-TO - Examples][2]
 - [Tomcat 8 JDBC Connection Pool Reference][3]
 - [Tomcat 8 Context Resource Links Reference][4]


  [1]: https://tomcat.apache.org/tomcat-8.0-doc/jndi-resources-howto.html#JDBC_Data_Sources
  [2]: https://tomcat.apache.org/tomcat-8.0-doc/jndi-datasource-examples-howto.html
  [3]: https://tomcat.apache.org/tomcat-8.0-doc/jdbc-pool.html
  [4]: https://tomcat.apache.org/tomcat-8.0-doc/config/context.html#Resource_Links



## JNDI Datasource for PostgreSQL & MySQL
Declare JNDI resource in tomcat's server.xml, using the Tomcat JDBC connection pool:

    <GlobalNamingResources>
        <Resource name="jdbc/DatabaseName"
                  factory="org.apache.tomcat.jdbc.pool.DataSourceFactory"
                  auth="Container"
                  type="javax.sql.DataSource"
                  username="dbUser"
                  password="dbPassword"
                  url="jdbc:postgresql://host/dbname"
                  driverClassName="org.postgresql.Driver"
                  initialSize="20"
                  maxWaitMillis="15000"
                  maxTotal="75"
                  maxIdle="20"
                  maxAge="7200000"
                  testOnBorrow="true"
                  validationQuery="select 1"
                  />
    </GlobalNamingResources>

And reference the JNDI resource from Tomcat's web context.xml:

      <ResourceLink name="jdbc/DatabaseName"
       global="jdbc/DatabaseName"
       type="javax.sql.DataSource"/>

If using MySQL, change URL, driver, and validation query:

      url="jdbc:mysql://host:3306/dbname"
      driverClassName="com.mysql.jdbc.Driver"
      validationQuery="/* ping */ SELECT 1"


## JNDI Encrypted credentials
In the JNDI declaration you may want to encrypt the username and password.

You have to implement a custom datasource factory in order to be able to decrypt the credentials.

In `server.xml` replace `factory="org.apache.tomcat.jdbc.pool.DataSourceFactory"` by `factory="cypher.MyCustomDataSourceFactory"`

Then define your custom factory :

    package cypher;

    import java.util.Enumeration;
    import java.util.Hashtable;

    import javax.naming.Context;
    import javax.naming.Name;
    import javax.naming.RefAddr;
    import javax.naming.Reference;
    import javax.naming.StringRefAddr;

    import org.apache.tomcat.dbcp.dbcp.BasicDataSourceFactory;

    public class MyCustomDataSourceFactory extends BasicDataSourceFactory {
        //This must be the same key used while encrypting the data
        private static final String ENC_KEY = "aad54a5d4a5dad2ad1a2";

        public MyCustomDataSourceFactory() {}

        @Override
        public Object getObjectInstance(final Object obj, final Name name, final Context nameCtx, final Hashtable environment) throws Exception {
            if (obj instanceof Reference) {
                setUsername((Reference) obj);
                setPassword((Reference) obj);
            }
            return super.getObjectInstance(obj, name, nameCtx, environment);
        }

        private void setUsername(final Reference ref) throws Exception {
            findDecryptAndReplace("username", ref);
        }

        private void setPassword(final Reference ref) throws Exception {
            findDecryptAndReplace("password", ref);
        }

        private void findDecryptAndReplace(final String refType, final Reference ref) throws Exception {
            final int idx = find(refType, ref);
            final String decrypted = decrypt(idx, ref);
            replace(idx, refType, decrypted, ref);
        }

        private void replace(final int idx, final String refType, final String newValue, final Reference ref) throws Exception {
            ref.remove(idx);
            ref.add(idx, new StringRefAddr(refType, newValue));
        }

        private String decrypt(final int idx, final Reference ref) throws Exception {
            return new CipherEncrypter(ENC_KEY).decrypt(ref.get(idx).getContent().toString());
        }

        private int find(final String addrType, final Reference ref) throws Exception {
            final Enumeration enu = ref.getAll();
            for (int i = 0; enu.hasMoreElements(); i++) {
                final RefAddr addr = (RefAddr) enu.nextElement();
                if (addr.getType().compareTo(addrType) == 0) {
                    return i;
                }
            }

            throw new Exception("The \"" + addrType + "\" name/value pair was not found" + " in the Reference object. The reference Object is" + " "
                    + ref.toString());
        }
    }


Of course you need an utility to encrypt the username and password ;

        package cypher;

    import java.io.UnsupportedEncodingException;
    import java.security.spec.AlgorithmParameterSpec;
    import java.security.spec.KeySpec;

    import javax.crypto.Cipher;
    import javax.crypto.IllegalBlockSizeException;
    import javax.crypto.SecretKey;
    import javax.crypto.SecretKeyFactory;
    import javax.crypto.spec.PBEKeySpec;
    import javax.crypto.spec.PBEParameterSpec;

    public class CipherEncrypter {

        Cipher ecipher;

        Cipher dcipher;

        byte[] salt = {
                (byte) 0xA9, (byte) 0x9B, (byte) 0xC8, (byte) 0x32, (byte) 0x56, (byte) 0x35, (byte) 0xE3, (byte) 0x03
        };

        int iterationCount = 19;

        /**
         * A java.security.InvalidKeyException with the message "Illegal key size or default parameters" means that the cryptography strength is limited; the unlimited strength juridiction policy files are not in the correct location. In a JDK,
         * they should be placed under ${jdk}/jre/lib/security
         * 
         * @param passPhrase
         */
        public CipherEncrypter(final String passPhrase) {
            try {
                // Create the key
                SecretKeyFactory factory = SecretKeyFactory.getInstance("PBEWithMD5AndDES");
                KeySpec spec = new PBEKeySpec(passPhrase.toCharArray(), salt, 65536, 256);
                SecretKey tmp = factory.generateSecret(spec);
                //            SecretKey secret = new SecretKeySpec(tmp.getEncoded(), "AES");

                // Create the ciphers
                ecipher = Cipher.getInstance(tmp.getAlgorithm());
                dcipher = Cipher.getInstance(tmp.getAlgorithm());

                final AlgorithmParameterSpec paramSpec = new PBEParameterSpec(salt, iterationCount);

                ecipher.init(Cipher.ENCRYPT_MODE, tmp, paramSpec);
                dcipher.init(Cipher.DECRYPT_MODE, tmp, paramSpec);

            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }

        public String encrypt(final String str) {
            try {
                final byte[] utf8 = str.getBytes("UTF8");
                byte[] ciphertext = ecipher.doFinal(utf8);
                return new sun.misc.BASE64Encoder().encode(ciphertext);
            }
            catch (final javax.crypto.BadPaddingException e) {
                //
            }
            catch (final IllegalBlockSizeException e) {
                //
            }
            catch (final UnsupportedEncodingException e) {
                //
            }
            catch (Exception e) {
                //
            }

            return null;
        }

        public String decrypt(final String str) {
            try {

                final byte[] dec = new sun.misc.BASE64Decoder().decodeBuffer(str);
                return new String(dcipher.doFinal(dec), "UTF-8");
            }
            catch (final javax.crypto.BadPaddingException e) {
                //TODO
            }
            catch (final IllegalBlockSizeException e) {
                //TODO
            }
            catch (final UnsupportedEncodingException e) {
                //TODO
            }
            catch (final java.io.IOException e) {
                //TODO
            }
            return null;
        }

        public static void main(final String[] args) {

            if (args.length != 1) {
                System.out.println("Error : you have to pass exactly one argument.");
                System.exit(0);
            }
            try {
                //This key is used while decrypting.
                final CipherEncrypter encrypter = new CipherEncrypter("aad54a5d4a5dad2ad1a2");
                final String encrypted = encrypter.encrypt(args[0]);
                System.out.println("Encrypted :" + encrypted);

                final String decrypted = encrypter.decrypt(encrypted);
                System.out.println("decrypted :" + decrypted);
            }
            catch (final Exception e) {

                e.printStackTrace();
            }

        }
    }

When you have encrypted values for username and password, replace the clear ones in `server.xml`.

Note that the encrypter should be in an obfuscated jar to keep the private key hidden (or you can also pass the key as an argument of the programm).

