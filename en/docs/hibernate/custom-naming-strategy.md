---
title: "Custom Naming Strategy"
slug: "custom-naming-strategy"
draft: false
images: []
weight: 9914
type: docs
toc: true
---

## Creating and Using a Custom ImplicitNamingStrategy

Creating a custom [`ImplicitNamingStrategy`][1] allows you to tweak how Hibernate will assign names to non-explicitly named `Entity` attributes, including Foreign Keys, Unique Keys, Identifier Columns, Basic Columns, and more.

For example, by default, Hibernate will generate Foreign Keys which are hashed and look similar to:

    FKe6hidh4u0qh8y1ijy59s2ee6m

While this is often not an issue, you may wish that the name was more descriptive, such as:

    FK_asset_tenant

This can easily be done with a custom `ImplicitNamingStrategy`. 

This example extends the [`ImplicitNamingStrategyJpaCompliantImpl`][2], however you may choose to implement [`ImplicitNamingStrategy`][1] if you wish.

    import org.hibernate.boot.model.naming.Identifier;
    import org.hibernate.boot.model.naming.ImplicitForeignKeyNameSource;
    import org.hibernate.boot.model.naming.ImplicitNamingStrategyJpaCompliantImpl;
    
    public class CustomNamingStrategy extends ImplicitNamingStrategyJpaCompliantImpl {
    
        @Override
        public Identifier determineForeignKeyName(ImplicitForeignKeyNameSource source) {
            return toIdentifier("FK_" + source.getTableName().getCanonicalName() + "_" + source.getReferencedTableName().getCanonicalName(), source.getBuildingContext());
        }
    
    }

To tell Hibernate which `ImplicitNamingStrategy` to use, define the `hibernate.implicit_naming_strategy` property in your `persistence.xml` or  `hibernate.cfg.xml` file as below:

    <property name="hibernate.implicit_naming_strategy"
                      value="com.example.foo.bar.CustomNamingStrategy"/>

Or you can specify the property in `hibernate.properties` file as below:

    hibernate.implicit_naming_strategy=com.example.foo.bar.CustomNamingStrategy

In this example, all Foreign Keys which do not have an explicitly defined `name` will now get their name from the `CustomNamingStrategy`.


  [1]: https://docs.jboss.org/hibernate/orm/5.1/javadocs/org/hibernate/boot/model/naming/ImplicitNamingStrategy.html
  [2]: https://docs.jboss.org/hibernate/orm/5.1/javadocs/org/hibernate/boot/model/naming/ImplicitNamingStrategyJpaCompliantImpl.html

## Custom Physical Naming Strategy
When mapping our entities to database table names we rely on a `@Table` annotation. But if we have a naming convention for our database table names, we can implement a custom physical naming strategy in order to tell hibernate to calculate table names based on the names of the entities, without explicitly stating those names with `@Table` annotation. Same goes for attributes and columns mapping.

For example, our entity name is:

    ApplicationEventLog

And our table name is:

    application_event_log

Our Physical naming strategy needs to convert from entity names that are camel case to our db table names which are snake case. We can achieve this by extending hibernate's `PhysicalNamingStrategyStandardImpl`:

    import org.hibernate.boot.model.naming.Identifier;
    import org.hibernate.boot.model.naming.PhysicalNamingStrategyStandardImpl;
    import org.hibernate.engine.jdbc.env.spi.JdbcEnvironment;
    
    public class PhysicalNamingStrategyImpl extends PhysicalNamingStrategyStandardImpl {
    
        private static final long serialVersionUID = 1L;
        public static final PhysicalNamingStrategyImpl INSTANCE = new PhysicalNamingStrategyImpl();
    
        @Override
        public Identifier toPhysicalTableName(Identifier name, JdbcEnvironment context) {
            return new Identifier(addUnderscores(name.getText()), name.isQuoted());
        }
    
        @Override
        public Identifier toPhysicalColumnName(Identifier name, JdbcEnvironment context) {
            return new Identifier(addUnderscores(name.getText()), name.isQuoted());
        }
    
        protected static String addUnderscores(String name) {
            final StringBuilder buf = new StringBuilder(name);
            for (int i = 1; i < buf.length() - 1; i++) {
                if (Character.isLowerCase(buf.charAt(i - 1)) &&
                        Character.isUpperCase(buf.charAt(i)) &&
                        Character.isLowerCase(buf.charAt(i + 1))) {
                    buf.insert(i++, '_');
                }
            }
            return buf.toString().toLowerCase(Locale.ROOT);
        }
    }

We are overriding default behavior of methods `toPhysicalTableName` and `toPhysicalColumnName` to apply our db naming convention.

In order to use our custom implementation we need to define `hibernate.physical_naming_strategy` property and give it the name of our `PhysicalNamingStrategyImpl` class.

    hibernate.physical_naming_strategy=com.example.foo.bar.PhysicalNamingStrategyImpl

This way we can alleviate our code from `@Table` and `@Column` annotations, so our entity class:

    @Entity
    public class ApplicationEventLog {
        private Date startTimestamp;
        private String logUser;
        private Integer eventSuccess;

        @Column(name="finish_dtl")
        private String finishDetails;
    }

will be correctly be mapped to db table:

    CREATE TABLE application_event_log (
      ...
      start_timestamp timestamp,
      log_user varchar(255),
      event_success int(11),
      finish_dtl varchar(2000),
      ...
    )

As seen in the example above, we can still explicitly state the name of the db object if it is not, for some reason, in accordance with our general naming convention:
    `@Column(name="finish_dtl")`

