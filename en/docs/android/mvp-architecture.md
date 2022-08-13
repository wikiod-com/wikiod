---
title: "MVP Architecture"
slug: "mvp-architecture"
draft: false
images: []
weight: 9585
type: docs
toc: true
---

This topic will provide [Model‑View‑Presenter (MVP)][MVP wiki] architecture of Android with various examples.

[MVP wiki]:https://www.wikiwand.com/en/Model%E2%80%93view%E2%80%93presenter

There are many ways to architect an Android app. But not all of them are testable and allows us to structure our code so that the app is easy to test. The key idea of a testable architecture is separating parts of the application which makes them easier to maintain, extend and test separately from each other. 

# MVP Definition

***Model***

In an application with a good layered architecture, this model would only be the gateway to the domain layer or business logic. See it as the provider of the data we want to display in the view.

***View***

The View, usually implemented by an `Activity` or `Fragment`, will contain a reference to the *presenter*. The only thing that the view will do is to call a method from the Presenter every time there is an interface action.

***Presenter***

The Presenter is responsible to act as the middle man between View and Model. It retrieves data from the Model and returns it formatted to the View. But unlike the typical MVC, it also decides what happens when you interact with the View.

\* Definitions from [Antonio Leiva’s article.][1]


# _Recommended_ App Structure (not required)
The app should be structured by package *per feature*. This improves readability and modularizes the app in a way that parts of it can be changed independently from each other. Each key feature of the app is in its own Java package.


  [1]: http://antonioleiva.com/mvp-android/

## Login example in the Model View Presenter (MVP) pattern
Let's see MVP in action using a simple Login Screen. There are two `Button`s—one for login action and another for a registration screen; two `EditText`s—one for the email and the other for the password.

**LoginFragment (The View)**

    public class LoginFragment extends Fragment implements LoginContract.PresenterToView, View.OnClickListener {
    
        private View view;
        private EditText email, password;
        private Button login, register;
    
        private LoginContract.ToPresenter presenter;
    
        @Nullable
        @Override
        public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
            return inflater.inflate(R.layout.fragment_login, container, false);
        }

        @Override
        public void onViewCreated(View view, @Nullable Bundle savedInstanceState) {
            email = (EditText) view.findViewById(R.id.email_et);
            password = (EditText) view.findViewById(R.id.password_et);
            login = (Button) view.findViewById(R.id.login_btn);
            login.setOnClickListener(this);
            register = (Button) view.findViewById(R.id.register_btn);
            register.setOnClickListener(this);
    
            presenter = new LoginPresenter(this);
    
            presenter.isLoggedIn();
    
        }
    
        @Override
        public void onLoginResponse(boolean isLoginSuccess) {
            if (isLoginSuccess) {
                startActivity(new Intent(getActivity(), MapActivity.class));
                getActivity().finish();
            }
        }
    
        @Override
        public void onError(String message) {
            Toast.makeText(getActivity(), message, Toast.LENGTH_SHORT).show();
        }
    
        @Override
        public void isLoggedIn(boolean isLoggedIn) {
            if (isLoggedIn) {
                startActivity(new Intent(getActivity(), MapActivity.class));
                getActivity().finish();
            }
        }
    
        @Override
        public void onClick(View view) {
            switch (view.getId()) {
                case R.id.login_btn:
                    LoginItem loginItem = new LoginItem();
                    loginItem.setPassword(password.getText().toString().trim());
                    loginItem.setEmail(email.getText().toString().trim());
                    presenter.login(loginItem);
                    break;
                case R.id.register_btn:
                    startActivity(new Intent(getActivity(), RegisterActivity.class));
                    getActivity().finish();
                    break;
            }
        }
    }

**LoginPresenter (The Presenter)**

    public class LoginPresenter implements LoginContract.ToPresenter {
    
        private LoginContract.PresenterToModel model;
        private LoginContract.PresenterToView view;
    
        public LoginPresenter(LoginContract.PresenterToView view) {
            this.view = view;
            model = new LoginModel(this);
        }
    
        @Override
        public void login(LoginItem userCredentials) {
            model.login(userCredentials);
        }
    
        @Override
        public void isLoggedIn() {
            model.isLoggedIn();
        }
    
        @Override
        public void onLoginResponse(boolean isLoginSuccess) {
            view.onLoginResponse(isLoginSuccess);
        }
    
        @Override
        public void onError(String message) {
            view.onError(message);
        }
    
        @Override
        public void isloggedIn(boolean isLoggedin) {
            view.isLoggedIn(isLoggedin);
        }
    }

**LoginModel (The Model)**

    public class LoginModel implements LoginContract.PresenterToModel, ResponseErrorListener.ErrorListener {
    
        private static final String TAG = LoginModel.class.getSimpleName();
        private LoginContract.ToPresenter presenter;
    
        public LoginModel(LoginContract.ToPresenter presenter) {
            this.presenter = presenter;
        }
    
        @Override
        public void login(LoginItem userCredentials) {
            if (validateData(userCredentials)) {
                try {
                    performLoginOperation(userCredentials);
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            } else {
                presenter.onError(BaseContext.getContext().getString(R.string.error_login_field_validation));
            }
        }
    
        @Override
        public void isLoggedIn() {
            DatabaseHelper database = new DatabaseHelper(BaseContext.getContext());
            presenter.isloggedIn(database.isLoggedIn());
        }
    
        private boolean validateData(LoginItem userCredentials) {
            return Patterns.EMAIL_ADDRESS.matcher(userCredentials.getEmail()).matches()
                    && !userCredentials.getPassword().trim().equals("");
        }
    
        private void performLoginOperation(final LoginItem userCredentials) throws JSONException {
    
            JSONObject postData = new JSONObject();
            postData.put(Constants.EMAIL, userCredentials.getEmail());
            postData.put(Constants.PASSWORD, userCredentials.getPassword());
    
            JsonObjectRequest request = new JsonObjectRequest(Request.Method.POST, Url.AUTH, postData,
                    new Response.Listener<JSONObject>() {
                        @Override
                        public void onResponse(JSONObject response) {
                            try {
                                String token = response.getString(Constants.ACCESS_TOKEN);
                                DatabaseHelper databaseHelper = new DatabaseHelper(BaseContext.getContext());
                                databaseHelper.login(token);
                                Log.d(TAG, "onResponse: " + token);
                            } catch (JSONException e) {
                                e.printStackTrace();
                            }
                            presenter.onLoginResponse(true);
                        }
                    }, new ErrorResponse(this));
    
            RequestQueue queue = Volley.newRequestQueue(BaseContext.getContext());
            queue.add(request);
        }
    
        @Override
        public void onError(String message) {
            presenter.onError(message);
        }
    }

# **Class Diagram**  
Let's see the action in the form of class diagram.
[![enter image description here][1]][1]

### Notes:

 - This example uses [Volley][2] for network communication, but this library is not required for MVP
 - `UrlUtils` is a class which contains all the links for my API Endpoints
 - `ResponseErrorListener.ErrorListener` is an `interface` that listens for error in `ErrorResponse` that `implements` Volley's `Response.ErrorListener`; these classes are not included here as they are not directly part of this example


  [1]: http://i.stack.imgur.com/yM4gJ.png
  [2]: https://developer.android.com/training/volley/index.html

## Simple Login Example in MVP
# Required package structure

[![Required package structure][1]][1]

# XML activity_login

    <LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:gravity="center_vertical"
        android:orientation="vertical"
        android:paddingBottom="@dimen/activity_vertical_margin"
        android:paddingLeft="@dimen/activity_horizontal_margin"
        android:paddingRight="@dimen/activity_horizontal_margin"
        android:paddingTop="@dimen/activity_vertical_margin">
    
        <EditText
            android:id="@+id/et_login_username"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:hint="USERNAME" />
    
        <EditText
            android:id="@+id/et_login_password"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:hint="PASSWORD" />
    
        <LinearLayout
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:orientation="horizontal">
    
            <Button
                android:id="@+id/btn_login_login"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:layout_marginRight="4dp"
                android:layout_weight="1"
                android:text="Login" />
    
            <Button
                android:id="@+id/btn_login_clear"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:layout_marginLeft="4dp"
                android:layout_weight="1"
                android:text="Clear" />
        </LinearLayout>
    
        <TextView
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_marginTop="3dp"
            android:text="correct user: mvp, mvp" />
    
        <ProgressBar
            android:id="@+id/progress_login"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:layout_marginTop="40dp" />
    
    </LinearLayout>

# Activity Class LoginActivity.class
    
    public class LoginActivity extends AppCompatActivity implements ILoginView, View.OnClickListener {
        private EditText editUser;
        private EditText editPass;
        private Button   btnLogin;
        private Button   btnClear;
        private ILoginPresenter loginPresenter;
        private ProgressBar progressBar;
    
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_login);
    
            //find view
            editUser = (EditText) this.findViewById(R.id.et_login_username);
            editPass = (EditText) this.findViewById(R.id.et_login_password);
            btnLogin = (Button) this.findViewById(R.id.btn_login_login);
            btnClear = (Button) this.findViewById(R.id.btn_login_clear);
            progressBar = (ProgressBar) this.findViewById(R.id.progress_login);
    
            //set listener
            btnLogin.setOnClickListener(this);
            btnClear.setOnClickListener(this);
    
            //init
            loginPresenter = new LoginPresenterCompl(this);
            loginPresenter.setProgressBarVisiblity(View.INVISIBLE);
        }
    
        @Override
        public void onClick(View v) {
            switch (v.getId()){
                case R.id.btn_login_clear:
                    loginPresenter.clear();
                    break;
                case R.id.btn_login_login:
                    loginPresenter.setProgressBarVisiblity(View.VISIBLE);
                    btnLogin.setEnabled(false);
                    btnClear.setEnabled(false);
                    loginPresenter.doLogin(editUser.getText().toString(), editPass.getText().toString());
                    break;
            }
        }
    
        @Override
        public void onClearText() {
            editUser.setText("");
            editPass.setText("");
        }
    
        @Override
        public void onLoginResult(Boolean result, int code) {
            loginPresenter.setProgressBarVisiblity(View.INVISIBLE);
            btnLogin.setEnabled(true);
            btnClear.setEnabled(true);
            if (result){
                Toast.makeText(this,"Login Success",Toast.LENGTH_SHORT).show();
            }
            else
                Toast.makeText(this,"Login Fail, code = " + code,Toast.LENGTH_SHORT).show();
        }
    
        @Override
        protected void onDestroy() {
            super.onDestroy();
        }
    
        @Override
        public void onSetProgressBarVisibility(int visibility) {
            progressBar.setVisibility(visibility);
        }
    }

# Creating an ILoginView Interface

Create an `ILoginView` interface for update info from Presenter under view folder as follows:

    public interface ILoginView {
        public void onClearText();
        public void onLoginResult(Boolean result, int code);
        public void onSetProgressBarVisibility(int visibility);
    }

# Creating an ILoginPresenter Interface

Create an `ILoginPresenter` interface in order to communicate with `LoginActivity` (Views) and create the `LoginPresenterCompl` class for handling login functionality and reporting back to the Activity. The `LoginPresenterCompl` class implements the `ILoginPresenter` interface:

## ILoginPresenter.class

    public interface ILoginPresenter {
        void clear();
        void doLogin(String name, String passwd);
        void setProgressBarVisiblity(int visiblity);
    }

## LoginPresenterCompl.class

    public class LoginPresenterCompl implements ILoginPresenter {
        ILoginView iLoginView;
        IUser user;
        Handler handler;
    
        public LoginPresenterCompl(ILoginView iLoginView) {
            this.iLoginView = iLoginView;
            initUser();
            handler = new Handler(Looper.getMainLooper());
        }
    
        @Override
        public void clear() {
            iLoginView.onClearText();
        }
    
        @Override
        public void doLogin(String name, String passwd) {
            Boolean isLoginSuccess = true;
            final int code = user.checkUserValidity(name,passwd);
            if (code!=0) isLoginSuccess = false;
            final Boolean result = isLoginSuccess;
            handler.postDelayed(new Runnable() {
                @Override
                public void run() {
                        iLoginView.onLoginResult(result, code);
                }
            }, 5000);
        }
    
        @Override
        public void setProgressBarVisiblity(int visiblity){
            iLoginView.onSetProgressBarVisibility(visiblity);
        }
    
        private void initUser(){
            user = new UserModel("mvp","mvp");
        }
    }

# Creating a UserModel

Create a `UserModel` which is like a Pojo class for `LoginActivity`. Create an `IUser` interface for Pojo validations:

## UserModel.class

    public class UserModel implements IUser {
    String name;
    String passwd;
    
    public UserModel(String name, String passwd) {
        this.name = name;
        this.passwd = passwd;
    }
    
    @Override
    public String getName() {
        return name;
    }
    
    @Override
    public String getPasswd() {
        return passwd;
    }
    
    @Override
    public int checkUserValidity(String name, String passwd){
        if (name==null||passwd==null||!name.equals(getName())||!passwd.equals(getPasswd())){
            return -1;
        }
        return 0;
    }

## IUser.class

    public interface IUser {
        String getName();
    
        String getPasswd();
    
        int checkUserValidity(String name, String passwd);
    }

# MVP

A Model-view-presenter (MVP) is a derivation of the model–view–controller (MVC) architectural pattern. It is used mostly for building user interfaces and offers the following benefits:

 - Views are more separated from Models. The Presenter is the mediator between
   Model and View.
 - It is easier to create unit tests.
 - Generally, there is a one-to-one mapping between View and Presenter,
   with the possibility to use multiple Presenters for complex Views.

[![Sketch of a Model-view-presenter][2]][2]

  [1]: https://i.stack.imgur.com/zO6Eg.png
  [2]: https://i.stack.imgur.com/br39x.png

