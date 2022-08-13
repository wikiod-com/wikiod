---
title: "Spring Security config with java (not XML)"
slug: "spring-security-config-with-java-not-xml"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Typical database backed, annotation base spring security setup.

## Syntax
 1. configureGlobal() configure the auth object.
 2. The later two SQLs may be optional.
 3. configure() method tells spring mvc how to authenticate request
 4. some url we do not need to authenticate
 5. others will redirect to /login if not yet authenticated.



## Basic spring security with annotation, SQL datasource
    @Configuration
    public class AppSecurityConfig extends WebSecurityConfigurerAdapter {

    @Autowired
    DataSource dataSource;

    @Autowired
    public void configureGlobal(AuthenticationManagerBuilder auth)
            throws Exception {       
         auth.jdbcAuthentication().dataSource(dataSource)
        .passwordEncoder(new BCryptPasswordEncoder())
        .usersByUsernameQuery("select username,password, enabled from users where username=?")
        .authoritiesByUsernameQuery("select username, role from user_roles where username=?");

    }
    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http.csrf().disable();
        http.authorizeRequests().antMatchers(".resources/**", "/public/**")
                .permitAll().anyRequest().authenticated().and().formLogin()
                .loginPage("/login").permitAll().and().logout().permitAll();

        }

    }

