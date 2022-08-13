---
title: "C# Authentication handler"
slug: "c-authentication-handler"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Authentication handler
     public class AuthenticationHandler : DelegatingHandler
        {
            /// <summary>
            /// Holds request's header name which will contains token.
            /// </summary>
            private const string securityToken = "__RequestAuthToken";
    
            /// <summary>
            /// Default overridden method which performs authentication.
            /// </summary>
            /// <param name="request">Http request message.</param>
            /// <param name="cancellationToken">Cancellation token.</param>
            /// <returns>Returns http response message of type <see cref="HttpResponseMessage"/> class asynchronously.</returns>
            protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                if (request.Headers.Contains(securityToken))
                {
                    bool authorized = Authorize(request);
                    if (!authorized)
                    {
                        return ApiHttpUtility.FromResult(request, false, HttpStatusCode.Unauthorized, MessageTypes.Error, Resource.UnAuthenticatedUser);
                    }
                }
                else
                {
                    return ApiHttpUtility.FromResult(request, false, HttpStatusCode.BadRequest, MessageTypes.Error, Resource.UnAuthenticatedUser);
                }
    
                return base.SendAsync(request, cancellationToken);
            }
    
            /// <summary>
            /// Authorize user by validating token.
            /// </summary>
            /// <param name="requestMessage">Authorization context.</param>
            /// <returns>Returns a value indicating whether current request is authenticated or not.</returns>
            private bool Authorize(HttpRequestMessage requestMessage)
            {
                try
                {
                    HttpRequest request = HttpContext.Current.Request;
                    string token = request.Headers[securityToken];
                    return SecurityUtility.IsTokenValid(token, request.UserAgent, HttpContext.Current.Server.MapPath("~/Content/"), requestMessage);
                }
                catch (Exception)
                {
                    return false;
                }
            }
        }

