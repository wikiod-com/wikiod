---
title: "Spring Security Configuration"
slug: "spring-security-configuration"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Configuration
Here is the corresponding Java configuration:

Add this annotation to an `@Configuration` class to have the Spring Security configuration defined in any `WebSecurityConfigurer` or more likely by extending the `WebSecurityConfigurerAdapter` base class and overriding individual methods:

<!-- language: java -->

    @Configuration
    @EnableWebSecurity
    @Profile("container")
    public class XSecurityConfig extends WebSecurityConfigurerAdapter {

**inMemoryAuthentication**  
It defines an in memory authentication scheme with a user that has the username "user", the password "password", and the role "ROLE_USER". 

<!-- language: java -->

      @Override
      protected void configure(AuthenticationManagerBuilder auth) throws Exception {
         auth
             .inMemoryAuthentication()
                 .withUser("user")
                 .password("password")
                 .roles("ROLE_USER");
      }

      @Override
      public void configure(WebSecurity web) throws Exception {
         web
             .ignoring()
                 .antMatchers("/scripts/**","/styles/**","/images/**","/error/**");
      }

**HttpSecurity**  
It allows configuring web based security for specific HTTP requests. By default it will be applied to all requests, but can be restricted using `requestMatcher(RequestMatcher)` or other similar methods. 

<!-- language: java -->

      @Override
      public void configure(HttpSecurity http) throws Exception {
         http
             .authorizeRequests()
                 .antMatchers("/rest/**").authenticated()
                 .antMatchers("/**").permitAll()
                 .anyRequest().authenticated()
                 .and()
             .formLogin()
                 .successHandler(new AuthenticationSuccessHandler() {
                     @Override
                     public void onAuthenticationSuccess(
                         HttpServletRequest request,
                         HttpServletResponse response,
                         Authentication a) throws IOException, ServletException {
                             // To change body of generated methods,
                             response.setStatus(HttpServletResponse.SC_OK);
                     }
                 })
                 .failureHandler(new AuthenticationFailureHandler() {
                     @Override
                     public void onAuthenticationFailure(
                         HttpServletRequest request,
                         HttpServletResponse response,
                         AuthenticationException ae) throws IOException, ServletException {
                             response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                     }
                 })
                 .loginProcessingUrl("/access/login")
                 .and()
             .logout()
                 .logoutUrl("/access/logout")                
                 .logoutSuccessHandler(new LogoutSuccessHandler() {
                     @Override
                     public void onLogoutSuccess(
                         HttpServletRequest request, 
                         HttpServletResponse response, 
                         Authentication a) throws IOException, ServletException {
                             response.setStatus(HttpServletResponse.SC_NO_CONTENT);
                     }
                 })
                 .invalidateHttpSession(true)
                 .and()
             .exceptionHandling()
             .authenticationEntryPoint(new Http403ForbiddenEntryPoint())
                 .and()
             .csrf() //Disabled CSRF protection
                 .disable();
      }
    }


