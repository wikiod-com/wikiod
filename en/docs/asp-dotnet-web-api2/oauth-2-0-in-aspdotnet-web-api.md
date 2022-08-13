---
title: "OAuth 2.0 in ASP.NET Web API"
slug: "oauth-20-in-aspnet-web-api"
draft: false
images: []
weight: 9907
type: docs
toc: true
---

**Registering in an Android Application**

These are the steps I've taken to log in / register using an Android app: 

 - Have a login activity which queries the ExternalLogins route, getting
   the available providers. This activity should have the NoHistory flag
   enabled and launch as a single instance.
 - On a user's button press, launch a Custom Chrome Tab with the
   provider's URL. The user should be logged in and redirected back to
   your published site at the given return URL. Don't use a WebView.
 - Have this page redirect the user again, using a custom URI scheme to
   launch a post-login activity within your application with the access
   token, expiry date and user account details added as additional data.
   This will need to be done in JavaScript on the web page, as the
   server's controllers can't access the URL parameters.
 - Store the user's details and token in a local MySQL database. On each
   login, check to see if the token is still in date.
 - Any calls to the API can now be authorized using the Authorization
   HTTP header, with your stored token added as so: “Bearer {token}”

## Configuring an OAuth Provider
You need to get some details from your OAuth provider of choice. We'll be looking at Google, but ASP.NET is also set up to allow out-the-box use of Twitter, Facebook and Microsoft (obviously). 

You'll want to go to the Google developer console (https://console.developers.google.com/) and create a project, enable the Google+ API (for getting the user's profile info, such as their name and avatar) and create a new OAuth 2 Client ID in the “Credentials” section. The authorized JavaScript origins should be your project's root URL (e.g. https://yourapi.azurewebsites.net) and the redirect URIs need to include ASP's built-in Google callback endpoint (https://yourapi.azurewebsites.net/signin-google) as well as your callback route of choice (https://yourapi.azurewebsites.net/callback). Getting these wrong will result in Google having a hissy fit. 

Back in your Visual Studio project, open App_Start > Startup.Auth.cs. Replace the commented Google section at the bottom with the code below, adding the ID and Secret from the Google Developers Console: 

    var googleAuthOptions = new GoogleOAuth2AuthenticationOptions()
    {
          ClientId = "YOUR ID",
          ClientSecret = "YOUR SECRET",
                Provider = new GoogleOAuth2AuthenticationProvider()
                {
                      OnAuthenticated = (context) =>
                      {
                            context.Identity.AddClaim(new Claim("urn:google:name", context.Identity.FindFirstValue(ClaimTypes.Name)));
                            context.Identity.AddClaim(new Claim("urn:google:email", context.Identity.FindFirstValue(ClaimTypes.Email)));
                            //This following line is need to retrieve the profile image
                            context.Identity.AddClaim(new Claim("urn:google:accesstoken", context.AccessToken, ClaimValueTypes.String, "Google"));
                            return System.Threading.Tasks.Task.FromResult(0);
                      }
                }
          };
    app.UseGoogleAuthentication(googleAuthOptions);

These additional claims allow you to query Google for the user's profile information, such as their name and avatar URL.

## Storing OAuth User Profiles
When someone registers with your application, a new ApplicationUser object will be stored in the database. By default the class is very barebones, but it can be customised - you can find it in Models > IdentityModels.cs. This is mine: 

    public class ApplicationUser : IdentityUser
    {
          public string ImageUrl { get; set; }
          public DateTime DateCreated { get; set; }
          public string FirstName { get; set; }
          public string AuthProvider { get; set; }
          public string Surname { get; set; }
    
          public async Task<ClaimsIdentity> GenerateUserIdentityAsync(UserManager<ApplicationUser> manager, string authenticationType)
          {
                // Note the authenticationType must match the one defined in CookieAuthenticationOptions.AuthenticationType
                var userIdentity = await manager.CreateIdentityAsync(this, authenticationType);
                // Add custom user claims here
                return userIdentity;
          }
    }

For reference, the user profile returned from Google with the Google+ API enabled takes the following JSON structure:

    {{
        "id": "1****************6",
        email": "dan********@gmail.com",
        "verified_email": true,
        "name": "Dan Richardson",
        "given_name": "Dan",
        "family_name": "Richardson",
        "link": "https://plus.google.com/+DanRichardson",
        "picture": "https://lh4.googleusercontent.com/photo.jpg",
        "gender": "male",
        "locale": "en"
    }}
                    

## Allowing Redirect URLs Other Than Site Root
Go to Providers > ApplicationOAuthProvider.cs and edit the ValidateClientRedirectUri function. This was a big gotcha to me, as if you don't do this there'll be a fantastically unhelpful error message. By default, this code will make any callbacks to your site invalid unless they're to the site's root. You likely want to be able to handle the callbacks in a controller, so you'll need to change it to something like this: 

    public override Task ValidateClientRedirectUri(OAuthValidateClientRedirectUriContext context)
    {
          if (context.ClientId == _publicClientId)
          {
                Uri expectedRootUri = new Uri(context.Request.Uri, "/");
                Uri expectedCallbackUri = new Uri(context.Request.Uri, "/callback");
    
                if (expectedRootUri.AbsoluteUri == context.RedirectUri ||
                      expectedCallbackUri.AbsoluteUri == context.RedirectUri)
                {
                      context.Validated();
                }
          }
          return Task.FromResult<object>(null);
    }

## Registration Flow
Here is the default flow of registering a user in Web API. All of these routes can be found in the AccountController:

 - The user requests a list of the login providers using the
   GetExternalLogins route, passing a return URL as a parameter. This
   returns an array of provider objects, containing the provider's name
   and the route that should be requested in order to log in with it
   (each configured to use the given return url). 
 
    e.g. GET:
   /api/Account/ExternalLogins?returnUrl=/callback&generateState=true,
   where the requested return URL is /callback
 - The user calls one of these returned URLs in a browser, where they're
   redirected to the provider's login page. Once logged in, the provider
   passes a cookie back to ASP, which handles the creation of an
   external user account. 
 - The user will be redirected to the return URL they passed in the
   first step. An external access token is passed back to the user,
   appended to the URL in a # param. This token can only be used on select
   routes, such as RegisterExternal.
 - The user now sends a POST request to RegisterExternal, using the new
   access token as a Bearer key. ASP then creates a new ApplicationUser
   and returns a proper access token which can be used on any route.

## Storing OAuth Profile Information
I have found that the Web API template is broken - the default implementation relies on cookies in the final step, which you probably don't want to be using in a Rest API. Without a cookie, GetExternalLoginInfoAsync in RegisterExternal always returns null.
I removed RegisterExternal entirely, instead creating the final user account in GetExternalLogin - called on return from the OAuth provider (in this case, Google):

    [OverrideAuthentication]
    [HostAuthentication(DefaultAuthenticationTypes.ExternalCookie)]
    [AllowAnonymous]
    [Route("ExternalLogin", Name = "ExternalLogin")]
    public async Task<IHttpActionResult> GetExternalLogin(string provider, string error = null)
    {
          if (error != null)
          {
                return Redirect(Url.Content("~/") + "#error=" + Uri.EscapeDataString(error));
          }
    
          if (!User.Identity.IsAuthenticated)
          {
                return new ChallengeResult(provider, this);
          }
    
          ExternalLoginData externalLogin = ExternalLoginData.FromIdentity(User.Identity as ClaimsIdentity);
    
          if (externalLogin == null)
          {
                return InternalServerError();
          }
    
          if (externalLogin.LoginProvider != provider)
          {
                Authentication.SignOut(DefaultAuthenticationTypes.ExternalCookie);
                return new ChallengeResult(provider, this);
          }
    
          ApplicationUser user = await UserManager.FindAsync(new UserLoginInfo(externalLogin.LoginProvider,
                    externalLogin.ProviderKey));
    
          bool hasRegistered = user != null;
    
          if (hasRegistered)
          {
                Authentication.SignOut(DefaultAuthenticationTypes.ExternalCookie);
                    
                ClaimsIdentity oAuthIdentity = await user.GenerateUserIdentityAsync(UserManager,
                        OAuthDefaults.AuthenticationType);
                ClaimsIdentity cookieIdentity = await user.GenerateUserIdentityAsync(UserManager,
                CookieAuthenticationDefaults.AuthenticationType);
    
                AuthenticationProperties properties = ApplicationOAuthProvider.CreateProperties(user.UserName);
                    Authentication.SignIn(properties, oAuthIdentity, cookieIdentity);
          }
          else
          {
                var accessToken = Authentication.User.Claims.Where(c => c.Type.Equals("urn:google:accesstoken")).Select(c => c.Value).FirstOrDefault();
                Uri apiRequestUri = new Uri("https://www.googleapis.com/oauth2/v2/userinfo?access_token=" + accessToken);
    
                RegisterExternalBindingModel model = new RegisterExternalBindingModel();
    
                using (var webClient = new System.Net.WebClient())
                {
                      var json = webClient.DownloadString(apiRequestUri);
                      dynamic jsonResult = JsonConvert.DeserializeObject(json);
                      model.Email = jsonResult.email;
                      model.Picture = jsonResult.picture;
                      model.Family_name = jsonResult.family_name;
                      model.Given_name = jsonResult.given_name;
                }
    
                var appUser = new ApplicationUser
                {
                      UserName = model.Email,
                      Email = model.Email,
                      ImageUrl = model.Picture,
                      FirstName = model.Given_name,
                      Surname = model.Family_name,
                      DateCreated = DateTime.Now
                };
    
                var loginInfo = await Authentication.GetExternalLoginInfoAsync();
    
                IdentityResult result = await UserManager.CreateAsync(appUser);
                if (!result.Succeeded)
                {
                      return GetErrorResult(result);
                }
    
                result = await UserManager.AddLoginAsync(appUser.Id, loginInfo.Login);
                if (!result.Succeeded)
                {
                      return GetErrorResult(result);
                }
    
                ClaimsIdentity oAuthIdentity = await appUser.GenerateUserIdentityAsync(UserManager,
                       OAuthDefaults.AuthenticationType);
                ClaimsIdentity cookieIdentity = await appUser.GenerateUserIdentityAsync(UserManager,
                        CookieAuthenticationDefaults.AuthenticationType);
    
                AuthenticationProperties properties = ApplicationOAuthProvider.CreateProperties(appUser.UserName);
                Authentication.SignIn(properties, oAuthIdentity, cookieIdentity);
          }
    
          return Ok();
    }

This removes the need for the client application to perform a needless POST request after receiving an external access token.

