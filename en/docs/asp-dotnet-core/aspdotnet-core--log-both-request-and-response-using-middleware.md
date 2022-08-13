---
title: "ASP.NET Core - Log both Request and Response using Middleware"
slug: "aspnet-core---log-both-request-and-response-using-middleware"
draft: false
images: []
weight: 9787
type: docs
toc: true
---

For some time I've searched for the best way to log requests and response in an ASP.Net Core. I was developing services and one of the requirements was to record request with its response in one record the the database. So many topics out there but none worked for me. it's either for request only, response only or simply didn't work.
When I was able to finally do it, and it had evolved during my project to better error handling and logging exceptions so I thought of sharing.

some of the topics that was helpful to me:

 - http://www.sulhome.com/blog/10/log-asp-net-core-request-and-response-using-middleware
 - http://dotnetliberty.com/index.php/2016/01/07/logging-asp-net-5-requests-using-middleware/
 - http://stackoverflow.com/questions/37855384/how-to-log-the-http-response-body-in-asp-net-core-1-0




## Logger Middleware
    using Microsoft.AspNetCore.Http;
    using System;
    using System.Diagnostics;
    using System.IO;
    using System.Linq;
    using System.Threading.Tasks;
    using Microsoft.AspNet.Http.Internal;
    using Microsoft.AspNetCore.Http.Internal;
    
      public class LoggerMiddleware
        {
            private readonly RequestDelegate _next;
    
            public LoggerMiddleware(RequestDelegate next)
            {
                _next = next;
            }
    
            public async Task Invoke(HttpContext context)
            {
                using (MemoryStream requestBodyStream = new MemoryStream())
                {
                    using (MemoryStream responseBodyStream = new MemoryStream())
                    {
                        Stream originalRequestBody = context.Request.Body;
                        context.Request.EnableRewind();
                        Stream originalResponseBody = context.Response.Body;
    
                        try
                        {
                            await context.Request.Body.CopyToAsync(requestBodyStream);
                            requestBodyStream.Seek(0, SeekOrigin.Begin);
    
                            string requestBodyText = new StreamReader(requestBodyStream).ReadToEnd();
    
                            requestBodyStream.Seek(0, SeekOrigin.Begin);
                            context.Request.Body = requestBodyStream;
    
                            string responseBody = "";
    
                    
                            context.Response.Body = responseBodyStream;
    
                            Stopwatch watch = Stopwatch.StartNew();
                            await _next(context);
                            watch.Stop();
    
                            responseBodyStream.Seek(0, SeekOrigin.Begin);
                            responseBody = new StreamReader(responseBodyStream).ReadToEnd();
                            AuditLogger.LogToAudit(context.Request.Host.Host,
                                context.Request.Path, context.Request.QueryString.ToString(), context.Connection.RemoteIpAddress.MapToIPv4().ToString(),
                                string.Join(",", context.Request.Headers.Select(he => he.Key + ":[" + he.Value + "]").ToList()),
                                requestBodyText, responseBody, DateTime.Now, watch.ElapsedMilliseconds);
    
                            responseBodyStream.Seek(0, SeekOrigin.Begin);
    
                            await responseBodyStream.CopyToAsync(originalResponseBody);
                        }
                        catch (Exception ex)
                        {
                            ExceptionLogger.LogToDatabse(ex);
                            byte[] data = System.Text.Encoding.UTF8.GetBytes("Unhandled Error occured, the error has been logged and the persons concerned are notified!! Please, try again in a while.");
                            originalResponseBody.Write(data, 0, data.Length);
                        }
                        finally
                        {
                            context.Request.Body = originalRequestBody;
                            context.Response.Body = originalResponseBody;
                        }
                    }
                }
            }
        }



