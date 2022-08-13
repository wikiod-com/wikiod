---
title: "Login Page"
slug: "login-page"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## SimpleLoginView

    public class SimpleLoginView extends CustomComponent implements View,
    Button.ClickListener {

    public static final String NAME = "login";

    private final TextField user;

    private final PasswordField password;

    private final Button loginButton;

    public SimpleLoginView() {
        setSizeFull();

        // Create the user input field
        user = new TextField("User:");
        user.setWidth("300px");
        user.setRequired(true);
        user.setInputPrompt("Your username (eg. joe@email.com)");
        user.addValidator(new EmailValidator(
                "Username must be an email address"));
        user.setInvalidAllowed(false);

        // Create the password input field
        password = new PasswordField("Password:");
        password.setWidth("300px");
        password.addValidator(new PasswordValidator());
        password.setRequired(true);
        password.setValue("");
        password.setNullRepresentation("");

        // Create login button
        loginButton = new Button("Login", this);

        // Add both to a panel
        VerticalLayout fields = new VerticalLayout(user, password, loginButton);
        fields.setCaption("Please login to access the application. (test@test.com/passw0rd)");
        fields.setSpacing(true);
        fields.setMargin(new MarginInfo(true, true, true, false));
        fields.setSizeUndefined();

        // The view root layout
        VerticalLayout viewLayout = new VerticalLayout(fields);
        viewLayout.setSizeFull();
        viewLayout.setComponentAlignment(fields, Alignment.MIDDLE_CENTER);
        viewLayout.setStyleName(Reindeer.LAYOUT_BLUE);
        setCompositionRoot(viewLayout);
    }

    @Override
    public void enter(ViewChangeEvent event) {
        // focus the username field when user arrives to the login view
        user.focus();
    }

    // Validator for validating the passwords
    private static final class PasswordValidator extends
            AbstractValidator<String> {

        public PasswordValidator() {
            super("The password provided is not valid");
        }

        @Override
        protected boolean isValidValue(String value) {
            //
            // Password must be at least 8 characters long and contain at least
            // one number
            //
            if (value != null
                    && (value.length() < 8 || !value.matches(".*\\d.*"))) {
                return false;
            }
            return true;
        }

        @Override
        public Class<String> getType() {
            return String.class;
        }
    }

    @Override
    public void buttonClick(ClickEvent event) {

        //
        // Validate the fields using the navigator. By using validors for the
        // fields we reduce the amount of queries we have to use to the database
        // for wrongly entered passwords
        //
        if (!user.isValid() || !password.isValid()) {
            return;
        }

        String username = user.getValue();
        String password = this.password.getValue();

        //
        // Validate username and password with database here. For examples sake
        // I use a dummy username and password.
        //
        boolean isValid = username.equals("test@test.com")
                && password.equals("passw0rd");

        if (isValid) {

            // Store the current user in the service session
            getSession().setAttribute("user", username);

            // Navigate to main view
            getUI().getNavigator().navigateTo(SimpleLoginMainView.NAME);//

        } else {

            // Wrong password clear the password field and refocuses it
            this.password.setValue(null);
            this.password.focus();

        }
    }
    }

## SimpleLoginUI
    public class SimpleLoginUI extends UI {

    @Override
    protected void init(VaadinRequest request) {

        //
        // Create a new instance of the navigator. The navigator will attach
        // itself automatically to this view.
        //
        new Navigator(this, this);

        //
        // The initial log view where the user can login to the application
        //
        getNavigator().addView(SimpleLoginView.NAME, SimpleLoginView.class);//

        //
        // Add the main view of the application
        //
        getNavigator().addView(SimpleLoginMainView.NAME,
                SimpleLoginMainView.class);

        //
        // We use a view change handler to ensure the user is always redirected
        // to the login view if the user is not logged in.
        //
        getNavigator().addViewChangeListener(new ViewChangeListener() {

            @Override
            public boolean beforeViewChange(ViewChangeEvent event) {

                // Check if a user has logged in
                boolean isLoggedIn = getSession().getAttribute("user") != null;
                boolean isLoginView = event.getNewView() instanceof SimpleLoginView;

                if (!isLoggedIn && !isLoginView) {
                    // Redirect to login view always if a user has not yet
                    // logged in
                    getNavigator().navigateTo(SimpleLoginView.NAME);
                    return false;

                } else if (isLoggedIn && isLoginView) {
                    // If someone tries to access to login view while logged in,
                    // then cancel
                    return false;
                }

                return true;
            }

            @Override
            public void afterViewChange(ViewChangeEvent event) {

            }
        });
    }
    }

## SImpleLoginMainView
    public class SimpleLoginMainView extends CustomComponent implements View {

    public static final String NAME = "";

    Label text = new Label();

    Button logout = new Button("Logout", new Button.ClickListener() {

        @Override
        public void buttonClick(ClickEvent event) {

            // "Logout" the user
            getSession().setAttribute("user", null);

            // Refresh this view, should redirect to login view
            getUI().getNavigator().navigateTo(NAME);
        }
    });

    public SimpleLoginMainView() {
        setCompositionRoot(new CssLayout(text, logout));
    }

    @Override
    public void enter(ViewChangeEvent event) {
        // Get the user name from the session
        String username = String.valueOf(getSession().getAttribute("user"));

        // And show the username
        text.setValue("Hello " + username);
    }
    }

