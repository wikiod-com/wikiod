---
title: "Creating A Custom ActionFilterAttribute"
slug: "creating-a-custom-actionfilterattribute"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Action Filters Attributes are a part of the ASP .NET Framework that I find useful to help follow the DRY principle. You can replace several lines of common logic with one simple declarative tag. The framework provides several useful Action Filter Attributes by default, such as the Authorize and Handle Error Attributes. This guide is intended to show you how to create your own custom attribute.

## EnsurePresenseOfAttribute
 This is an example of an attribute that I created to validate that required parameters have been assigned in the request object receive in a POST route. I decided on this approach because the standard [ModelState.IsValid][1] approach was not valid. This is because the required attributes vary based on what action is being called.

// ATTRIBUTE ONLY VALID FOR METHODS
[AttributeUsage(AttributeTargets.Method)]
// INHERIT ActionFilterAttribute
public class EnsurePresencesOfAttribute : ActionFilterAttribute
{
    // ReSharper disable once InconsistentNaming
    public string required { get; set; }

    // VALIDATE REQUIRED ATTRIBUTES
    // FOR NON-ASYNC REQUESTS
    public override void OnActionExecuting(HttpActionContext context)
    {
        Dictionary<string, object> model = context.ActionArguments;
        var serialstring = JsonConvert.SerializeObject(model);
        foreach (var requirement in required.Split(','))
        {
            if (serialstring.Contains($"{requirement}\":null"))
            {
                ValueError(context, requirement);
                return;
            }
        }
        base.OnActionExecuting(context);
    }

    // VALIDATE THE REQUIRED ATTRIBUTES ARE PRESENT
    // FOR ASYNC REQUESTS
    public override Task OnActionExecutingAsync(HttpActionContext context, CancellationToken token)
    {
        Dictionary<string, object> model = context.ActionArguments;
        var serialstring = JsonConvert.SerializeObject(model);
        foreach (var requirement in required.Split(','))
        {
            if (serialstring.Contains($"{requirement}\":null"))
            {
                ValueError(context, requirement);
                return Task.FromResult(0);
            }
        }
        return base.OnActionExecutingAsync(context, token);
    }

    // LOG ERROR AND RETURN AND SET ERROR RESPONSE
    private static void ValueError(HttpActionContext context, string requirement)
    {
        var action = context.ActionDescriptor.ActionName;
        AppUtils.LogError($"{action} Failed : Missing Required Attribute {requirement}. ");
        using (var controller = new BaseApiController { Request = new HttpRequestMessage() })
        {
            controller.Request.Properties.Add(HttpPropertyKeys.HttpConfigurationKey, new HttpConfiguration());
            context.Response = controller.InvalidInputResponse();
        }
    }



  [1]: https://forums.asp.net/t/1994514.aspx?About%20ModelState%20IsValid

## Controller Before EnsuresPresenseOf Attribute
            [HttpPost]
            [Route("api/Fitbit/Activity/Stats")]
            public async Task<HttpResponseMessage> ActivityStats(FitbitRequestDTO request)
            {
                if (string.IsNullOrEmpty(request.PatientId) || string.IsNullOrEmpty(request.DeviceId))
                    return InvalidInputResponse();
                try
                {
                    var tokenErrorResponse = await EnsureToken(request);
                    if (tokenErrorResponse != null)
                        return tokenErrorResponse;
                    var client = GetFitbitClient();
                    var stats = await client.GetActivitiesStatsAsync();
                    return OkResponse(stats);
                }
                catch (Exception e)
                {
                    const string function = " ActivityStats ";
                    AppUtils.LogException(function, e);
                    return SystemErrorResponse(function, e);
                }
            }

## Update Controller
            public async Task<HttpResponseMessage> ActivityStats(FitbitRequestDTO request)
            {
                try
                {
                    var tokenErrorResponse = await EnsureToken(request);
                    if (tokenErrorResponse != null)
                        return tokenErrorResponse;
                    var client = GetFitbitClient();
                    var stats = await client.GetActivitiesStatsAsync();
                    return OkResponse(stats);
                }
                catch (Exception e)
                {
                    const string function = " ActivityStats ";
                    AppUtils.LogException(function, e);
                    return SystemErrorResponse(function, e);
                }
            }

