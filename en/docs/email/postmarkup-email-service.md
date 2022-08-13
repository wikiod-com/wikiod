---
title: "Postmarkup email service"
slug: "postmarkup-email-service"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Postmark up is a third party API which provide you to send mail or we can say multiple mails at a time with a speedy response.


## Send mail through postmark email API.
**PostmarkMailSender.java**

    public class PostmarkMailSender implements MailSender{
    
        private static Logger logger = Logger.getLogger("com.postmark");
    
        private String serverToken;
        private static Gson gson;
    
        static {
            GsonBuilder gsonBuilder = new GsonBuilder();
            gsonBuilder.disableHtmlEscaping();
            SimpleMailMessage src=new SimpleMailMessage();
            String [] arr={"",""};
            src.setTo(arr);
            System.out.println(src.getTo());
    
            gsonBuilder.registerTypeAdapter(SimpleMailMessage.class, new SimpleMailMessageAdapter());
            gsonBuilder.registerTypeAdapter(PostmarkMessage.class, new SimpleMailMessageAdapter());
            gsonBuilder.registerTypeAdapter(DateTime.class, new DateTimeTypeAdapter());
            gson = gsonBuilder.create();
        }
    
    
        /**
         * Initializes a new instance of the PostmarkClient class.
         * <p/>
         * If you do not have a server token you can request one by signing up to
         * use Postmark: http://postmarkapp.com.
         *
         * @param serverToken the postmark server token
         */
        public PostmarkMailSender(String serverToken) {
            this.serverToken = serverToken;
        }
    
        @Override
        public void send(SimpleMailMessage message) throws MailException {
    
            System.out.println("1*************************************************");
    
            HttpClient httpClient = new DefaultHttpClient();
            PostmarkResponse theResponse = new PostmarkResponse();
            System.out.println("2***************************************");
    
            try {
                System.out.println("3********************************************");
                // Create post request to Postmark API endpoint
                HttpPost method = new HttpPost("http://api.postmarkapp.com/email");
    
                // Add standard headers required by Postmark
                method.addHeader("Accept",            "application/json");
                method.addHeader("Content-Type",    "application/json; charset=utf-8");
                method.addHeader("X-Postmark-Server-Token", serverToken);
                method.addHeader("User-Agent",        "Postmark-Java");
    
                // Convert the message into JSON content
                String messageContents = UnicodeEscapeFilterWriter.escape(gson.toJson(message));
                logger.log(Level.FINER, "Message contents: " + messageContents);
    
                // Add JSON as payload to post request
                StringEntity payload = new StringEntity(messageContents);
                payload.setContentEncoding(HTTP.UTF_8);
                method.setEntity(payload);
                System.out.println("4********************************************");
                ResponseHandler<String> responseHandler = new BasicResponseHandler();
                try {
                    System.out.println("5*************************************************");
    
                    String response = httpClient.execute(method, responseHandler);
                    logger.log(Level.FINER, "Message response: " + response);
                    theResponse = gson.fromJson(response, PostmarkResponse.class);
                    theResponse.status = PostmarkResponseStatus.SUCCESS;
                } catch (HttpResponseException hre) {
                    switch(hre.getStatusCode()) {
                        case 401:
                        case 422:
                            logger.log(Level.SEVERE, "There was a problem with the email: " + hre.getMessage());
                            theResponse.setMessage(hre.getMessage());
                            theResponse.status = PostmarkResponseStatus.USERERROR;
                            throw new MailSendException("Postmark returned: "+theResponse);
                        case 500:
                            logger.log(Level.SEVERE, "There has been an error sending your email: " + hre.getMessage());
                            theResponse.setMessage(hre.getMessage());
                            theResponse.status = PostmarkResponseStatus.SERVERERROR;
                            throw new MailSendException("Postmark returned: "+theResponse);
                        default:
                            logger.log(Level.SEVERE, "There has been an unknow error sending your email: " + hre.getMessage());
                            theResponse.status = PostmarkResponseStatus.UNKNOWN;
                            theResponse.setMessage(hre.getMessage());
                            throw new MailSendException("Postmark returned: "+theResponse);
    
    
                    }
                }
    
            } catch (Exception e) {
                logger.log(Level.SEVERE, "There has been an error sending email: " + e.getMessage());
                throw new MailSendException("There has been an error sending email", e);
    
            } finally {
                httpClient.getConnectionManager().shutdown();
            }
    
    
        }
    
        @Override
        public void send(SimpleMailMessage[] simpleMessages) throws MailException {
            System.out.println("7*************************************************");
            ///FIXME default naive, non-optimal implementation (sequentially opens simpleMessages.length HTTP connections...)
            Map<Object, Exception> failedMessages = new LinkedHashMap<Object, Exception>();
            for(SimpleMailMessage simpleMessage: simpleMessages) {
                try {
                    System.out.println("8*************************************************");
                    send(simpleMessage);
                } catch (MailException mex) {
                    failedMessages.put(simpleMessage, mex);
                }
            }
            if(! failedMessages.isEmpty())
                throw new MailSendException(failedMessages);
        }
    
    
    
        //  PostMark Reponse utilities
    
        /**
         * Possible outcomes of a Response from the Postmark server
         */
        static enum PostmarkResponseStatus {
            UNKNOWN, SUCCESS, USERERROR, SERVERERROR
        }
    
        /**
         * Response from the Postmark server
         */
        static class PostmarkResponse {
            @Override
            public String toString() {
                return "PostmarkResponse [errorCode=" + errorCode + ", message="
                        + message + ", status=" + status + ", submittedAt="
                        + submittedAt + ", to=" + to + "]";
            }
    
            /** The status outcome of the response. */
            @SerializedName("Status")
            public PostmarkResponseStatus status;
    
            /** The message from the API. In the event of an error, this message may contain helpful text. */
            @SerializedName("Message")
            public String message;
    
            /** The time the request was received by Postmark. */
            @SerializedName("SubmittedAt")
            public DateTime submittedAt;
    
            /** The recipient of the submitted request. */
            @SerializedName("To")
            public String to;
    
            /** The error code returned from Postmark. This does not map to HTTP status codes. */
            @SerializedName("ErrorCode")
            public int errorCode;
    
            public PostmarkResponseStatus getStatus() {
                return status;
            }
            public void setStatus(PostmarkResponseStatus status) {
                this.status = status;
            }
    
            public String getMessage() {
                return message;
            }
            public void setMessage(String message) {
                this.message = message;
            }
    
            public DateTime getSubmittedAt() {
                return submittedAt;
            }
            public void setSubmittedAt(DateTime submittedAt) {
                this.submittedAt = submittedAt;
            }
    
            public String getTo() {
                return to;
            }
            public void setTo(String to) {
                this.to = to;
            }
    
            public int getErrorCode() {
                return errorCode;
            }
            public void setErrorCode(int errorCode) {
                this.errorCode = errorCode;
            }
        }
    
    
    
        //  GSON Serializers
    
        /**
         * Gson Serializer for Spring's SimpleMailMessage
         *
         * @see JsonSerializer
         * @see SimpleMailMessage
         */
        public static class SimpleMailMessageAdapter implements JsonSerializer<SimpleMailMessage> {
    
            @Override
            public JsonElement serialize(SimpleMailMessage src, Type typeOfSrc, JsonSerializationContext context) {
                System.out.println("9*************************************************");
                JsonObject jsonO = new JsonObject();
                if (src.getFrom() == null) {
                    throw new MailParseException("You must specify a from address");
                }
                jsonO.addProperty("From", "support@pharmerz.in");
                if (src.getTo() == null) {
                    throw new MailParseException("You must specify a to address");
                }
                String [] arrnew=src.getTo();
    
                jsonO.addProperty("To",arrnew[0]);
                if (src.getCc() != null) {
                    jsonO.addProperty("Cc", "");
                }
                if (src.getBcc() != null) {
                    jsonO.addProperty("Bcc", "");
                }
                if (src.getSubject() == null) {
                    throw new MailParseException("You must specify a Subject field");
                }
                jsonO.addProperty("Subject",src.getSubject());
    
                if (src instanceof PostmarkMessage) {
                    PostmarkMessage postmarkSrc = (PostmarkMessage) src;
    
                    if (postmarkSrc.getTag() != null) {
                        jsonO.addProperty("Tag", postmarkSrc.getTag());
                    }
    
                    if (postmarkSrc.getHtmlBody() != null) {
                        jsonO.addProperty("HtmlBody", ((PostmarkMessage) src).getHtmlBody());
                    }
    
                    if (src.getText() == null && postmarkSrc.getHtmlBody() == null) {
                        throw new MailParseException("You must specify a Text field !");
                    }
    
                } else  if (src.getText() == null) {
                    throw new MailParseException("You must specify a Text field");
                }
    
                jsonO.addProperty("TextBody", "test mail from postmarkmailsender");
    
                if (src.getReplyTo() != null) {
                    jsonO.addProperty("ReplyTo", "kk");
                }
    
                return jsonO;
            }
    
            private static String mergeMailAddresses(String[] addresses) {
                StringBuilder sb = new StringBuilder();
                for (int i = 0; i < addresses.length; i++) {
                    sb.append(addresses[i]);
                    if (i < addresses.length - 1) {
                        sb.append(",");
                    }
                }
                return sb.toString();
            }
    
        }
    
        /**
         * Gson Serializer for Joda DateTime
         *
         * @see JsonSerializer
         * @see DateTime
         */
        static class DateTimeTypeAdapter implements JsonSerializer<DateTime>, JsonDeserializer<DateTime> {
            @Override
            public JsonElement serialize(DateTime src, Type typeOfSrc, JsonSerializationContext context) {
                return new JsonPrimitive(src.toString());
            }
    
            @Override
            public DateTime deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
                return new DateTime(json.getAsJsonPrimitive().getAsString());
            }
        }

**PostmarkMessage.java**

    public class PostmarkMessage extends SimpleMailMessage  {
    
    
        private String tag;
    
        private String htmlBody;
    
        public void setTag(String tag) {
            this.tag = tag;
        }
    
        public String getTag() {
            return tag;
        }
    
        public String getHtmlBody() {
            return htmlBody;
        }
    
        public void setHtmlBody(String htmlBody) {
            this.htmlBody = htmlBody;
        }
    }


**UnicodeEscapeFilterWriter.java**


    public class UnicodeEscapeFilterWriter extends FilterWriter{
    
    
        protected UnicodeEscapeFilterWriter(Writer out) {
            super(out);
        }
    
        private static char[] prefix = { '\\', 'u', '0', '0', '0' };
    
    
        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            for (int i = 0; i < len; i++) {
                if ((cbuf[i] > '\u007f')) {
                    String hx = Integer.toHexString(cbuf[i]);
                    out.write(prefix, 0, 6 - hx.length());
                    out.write(hx);
                } else
                    out.write(cbuf[i]);
            }
        }
    
        @Override
        public void write(int c) throws IOException {
            write(new char[] {(char)c}, 0, 1);
        }
    
        @Override
        public void write(String str, int off, int len) throws IOException {
            write(str.toCharArray(), off, len);
        }
    
    
        /**
         * Escapes <tt>str</tt> using this Filter.
         * <p>
         * Default estimate for non-ASCII chars ratio is 3%
         *
         * @param str the original String
         * @return the escaped String
         * @throws IOException
         */
        public static String escape(String str) throws IOException {
            return escape(str, .03f);
        }
    
        /**
         * Escapes <tt>str</tt> using this Filter.
         *
         * @param str the original String
         * @param estimatedNonASCIIRatio an estimation of the ratio of non-ASCII chars in <tt>str</tt>.
         * It is used for the initial allocation size of the new char array.
         * @return the escaped String
         * @throws IOException
         */
        public static String escape(String str, float estimatedNonASCIIRatio) throws IOException {
            StringWriter out = new StringWriter(Math.round(str.length() * (1 + estimatedNonASCIIRatio * 5)));
            UnicodeEscapeFilterWriter fw = new UnicodeEscapeFilterWriter(out);
            fw.write(str);
            return out.toString();
    
        }
    }


Implementation now over now we can build method which will send email on awaking

**Email.java**

    public class Email {
    
    
        public void SendMail(String TO, String SUBJECT, String HTMLBODY) {
    
            String TEST_API_KEY = "3b2e23b0-****-****-****-*********";
    
            String FROM = "support@pharmerz.in";
            PostmarkMailSender mailSender = new PostmarkMailSender(TEST_API_KEY);
    
            PostmarkMessage m = new PostmarkMessage();
            m.setFrom(FROM);
            m.setTo(TO);
            m.setSubject(SUBJECT);
            m.setHtmlBody(HTMLBODY);
            //m.setText(HTMLBODY);
            // m.setTag("test-utf8");
            mailSender.send(m);
        }
    
    }




