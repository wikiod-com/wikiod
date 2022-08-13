---
title: "Make a SurfaceController to allow members to log in"
slug: "make-a-surfacecontroller-to-allow-members-to-log-in"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

The code was written and tested on Umbraco 7.5.3. 

## Add ViewModel
First we need to make a ViewModel. Make a new folder /Models/Account, and add the file MemberLoginViewModel.cs to the folder.

    using System;
    using System.Collections.Generic;
    using System.ComponentModel.DataAnnotations;
    using System.Linq;
    using System.Web;
    
    namespace MyCMS.Models.Account
    {
        public class MemberLoginViewModel
        {
            [Required]
            [EmailAddress]
            [Display(Name = "Email")]
            public string Email { get; set; }
            
            [Required]
            [DataType(DataType.Password)]
            [Display(Name = "Password")]
            public string Password { get; set; }
    
            [Display(Name = "Remember me?")]
            public bool RememberMe { get; set; }
        }
    }

## Add a view
Then make a view in the folder /Views/MacroPartials/Account

    @model MyCMS.Models.Account.MemberLoginViewModel
    @using MyCMS.Controllers.Account
    
    @if (User.Identity.IsAuthenticated)
    {
            <p>Logged in as: @User.Identity.Name</p>
            using (Html.BeginUmbracoForm<MemberLoginSurfaceController>("MemberLogout", FormMethod.Post, new { id = "logoutForm" }))
            {
                @Html.AntiForgeryToken()
             <button type="submit" class="btn btn-outline-secondary">Log out</button>
             }
    
    }
    else
    {
        using (Html.BeginUmbracoForm<MemberLoginSurfaceController>("MemberLoginPost", "MemberLoginSurface", new { @class = "form-inline" }))
        {
            <div class="form-group">
                <label for="email">Email</label>
                <input name="Email" type="email" class="form-control" id="loginform_email" placeholder="name@email.no">
            </div>
                    <div class="form-group">
                        <label for="password">Password</label>
                        <input name="Password" type="password" class="form-control" id="loginform_password" placeholder="*****">
                    </div>
                    <div class="form-check">
                        <label class="form-check-label">
                            <input name="RememberMe" class="form-check-input" type="checkbox">Husk meg
                        </label>
                    </div>
                    <button type="submit" class="btn btn-outline-secondary">Log in</button>
    
        }
        <p>@TempData["Status"]</p>
    }  

## Add controller
Make a new controller in the folder /Controllers/Account. Name the file MemberLoginSurfaceController.cs

    using MyCMS.Models.Account;
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Web;
    using System.Web.Mvc;
    using System.Web.Security;
    
    namespace MyCMS.Controllers.Account
    {
        public class MemberLoginSurfaceController : Umbraco.Web.Mvc.SurfaceController
        {
            // Inspired by: http://24days.in/umbraco/2012/creating-a-login-form-with-umbraco-mvc-surfacecontroller/
    
            // GET: MemberLoginSurface
            [HttpGet]
            [ActionName("MemberLoginForm")]
            [ChildActionOnly]
            public ActionResult MemberLoginForm()
            {
                return PartialView("Components/MemberLogin", new MemberLoginViewModel());
            }
    
            // The MemberLogout Action signs out the user and redirects to the site home page:
            [HttpPost]
            [ValidateAntiForgeryToken]
            public ActionResult MemberLogout()
            {
                TempData["Message"] = "You have logged out!";
                Session.Clear();
                FormsAuthentication.SignOut();
                return RedirectToCurrentUmbracoPage();
            }
    
            // The MemberLoginPost Action checks the entered credentials using the standard Asp Net membership provider and redirects the user to the same page. Either as logged in, or with a message set in the TempData dictionary:
    
            [HttpPost]
            [ActionName("MemberLoginPost")]
            public ActionResult MemberLoginPost(MemberLoginViewModel model)
            {
    
                if (Membership.ValidateUser(model.Email, model.Password))
                {
                    FormsAuthentication.SetAuthCookie(model.Email, model.RememberMe);
                    return RedirectToCurrentUmbracoPage();
                }
                else
                {
                    TempData["Status"] = "Invalid username or password";
                    return RedirectToCurrentUmbracoPage();
                }
            }
        }
    }

## Show me the login form!
To render your new component in a view, you cust call:

    @Html.Action("MemberLoginForm", "MemberLoginSurface")

