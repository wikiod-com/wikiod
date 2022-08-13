---
title: "How to use FirebaseRecyclerAdapter instead of RecyclerAdapter?"
slug: "how-to-use-firebaserecycleradapter-instead-of-recycleradapter"
draft: false
images: []
weight: 9890
type: docs
toc: true
---

## Here is the Example for  Use FirebaseUi component FirebaseRecyclerAdapter
Hello friends before start code we have need to declare dependency for access firebase ui component, so here is the dependency which you can put it in your gradel other wise you can add dependency as jar also.

    compile 'com.firebaseui:firebase-ui-database:0.4.0'
Then after we are querying in firebase database for data like following way 

    DatabaseReference databaseReference = database.getReference().child("users");
    Query query = databaseReference.limitToFirst(50); 

Then after we pass query inside of FirebaseRecyclerAdapter like following way 

    private void setUpFirebaseAdapter(Query query) {
    
            mFirebaseAdapter = new FirebaseRecyclerAdapter<UserModel, FirebaseUserViewHolder>
                    (UserModel.class, R.layout.row_user_list, FirebaseUserViewHolder.class, query) {
                @Override
                protected void populateViewHolder(FirebaseUserViewHolder viewHolder, UserModel model, int position) {
                    customeLoaderDialog.hide();
                    viewHolder.bindUser(model);
                }
            };
    
            my_recycler_view.setHasFixedSize(true);
            my_recycler_view.setLayoutManager(new LinearLayoutManager(this));
            my_recycler_view.setAdapter(mFirebaseAdapter);
    
        }


ChatUserModel.java (Model Class) 
        
    public class ChatUserModel {
    private long badge;
    private String chat_id;
    private String isDelete;
    private String latestactivity;
    private double timestamp;
    private String user_id;
    private String profilePic;
    private String displayName;
    private boolean isGroup;
    String groupId;
    private String creatorId;

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public String getCreatorId() {
        return creatorId;
    }

    public void setCreatorId(String creatorId) {
        this.creatorId = creatorId;
    }

    public boolean isGroup() {
        return isGroup;
    }

    public void setGroup(boolean group) {
        isGroup = group;
    }


    public ChatUserModel() {

    }

    public long getBadge() {
        return badge;
    }

    public void setBadge(long badge) {
        this.badge = badge;
    }

    public String getChat_id() {
        return chat_id;
    }

    public void setChat_id(String chat_id) {
        this.chat_id = chat_id;
    }

    public String getIsDelete() {
        return isDelete;
    }

    public void setIsDelete(String isDelete) {
        this.isDelete = isDelete;
    }

    public String getLatestactivity() {
        return latestactivity;
    }

    public void setLatestactivity(String latestactivity) {
        this.latestactivity = latestactivity;
    }

    public double getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(double timestamp) {
        this.timestamp = timestamp;
    }

    public String getUser_id() {
        return user_id;
    }

    public void setUser_id(String user_id) {
        this.user_id = user_id;
    }

    public String getProfilePic() {
        return profilePic;
    }

    public void setProfilePic(String profilePic) {
        this.profilePic = profilePic;
    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }}

FirebaseChatUserViewHolder.java (Recycler ViewHolder)

    public class FirebaseChatUserViewHolder extends RecyclerView.ViewHolder implements View.OnClickListener {
    
        private static final int MAX_WIDTH = 200;
        private static final int MAX_HEIGHT = 200;
        View mView;
        Context mContext;
        ChatUserModel userModel;
    
        public FirebaseChatUserViewHolder(View itemView) {
            super(itemView);
            mView = itemView;
            mContext = itemView.getContext();
            itemView.setOnClickListener(this);
        }
    
        public void bindUser(ChatUserModel userModel) {
            this.userModel = userModel;
            ImageView imgUser = (ImageView) mView.findViewById(R.id.imgUser);
            TextView tvName = (TextView) mView.findViewById(R.id.tvName);
            TextView tvStatus = (TextView) mView.findViewById(R.id.tvStatus);
            BadgeView badgeChat = (BadgeView) mView.findViewById(R.id.badgeChat);
            if (userModel.isGroup()) {
               // imgUser.setImageDrawable(mContext.getResources().getDrawable(R.drawable.create_group));
            } else {
                Picasso.with(mContext)
                        .load(userModel.getProfilePic())
                        .resize(MAX_WIDTH, MAX_HEIGHT)
                        .centerCrop()
                        .into(imgUser);
            }
    
            tvName.setText(userModel.getDisplayName());
            tvStatus.setText(userModel.getLatestactivity());
            if (userModel.getBadge() > 0) {
                badgeChat.setVisibility(View.VISIBLE);
                badgeChat.setText("" + userModel.getBadge());
            } else {
                badgeChat.setVisibility(View.GONE);
            }
    
        }
    
        @Override
        public void onClick(View view) {
            if (!userModel.isGroup()) {
                Intent intent = new Intent(mContext, ChatConverstion.class);
                intent.putExtra("chat_id", "" + userModel.getChat_id());
                intent.putExtra("reciverUserName", "" + userModel.getDisplayName());
                intent.putExtra("reciverProfilePic", "" + userModel.getProfilePic());
                intent.putExtra("reciverUid", "" + userModel.getUser_id());
                mContext.startActivity(intent);
            }
        }
    }

row_user_list.xml (layout for row in recycler view)

    <RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:background="@android:color/white"
        android:orientation="horizontal"
       >
    
        <LinearLayout
            android:gravity="center_vertical"
            android:layout_width="match_parent"
            android:id="@+id/llMainChat"
            android:layout_height="wrap_content"
            android:orientation="horizontal"
            android:paddingTop="@dimen/margin_small"
            android:paddingLeft="@dimen/margin_small"
            android:paddingBottom="@dimen/margin_small"
            android:paddingRight="@dimen/margin_small"
            >
    
            <com.tristate.firebasechat.custome_view.CircleImageView
                android:id="@+id/imgUser"
                android:layout_width="@dimen/tab_top_height"
                android:layout_height="@dimen/tab_top_height"
    
                app:civ_border_color="@color/dark_white"
                app:civ_border_width="2dp" />
    
            <RelativeLayout
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:orientation="horizontal"
                android:layout_marginLeft="@dimen/margin_medium"
             >
    
                <LinearLayout
                    android:layout_width="match_parent"
                    android:layout_height="wrap_content"
                    android:orientation="vertical"
                    android:layout_toLeftOf="@+id/badgeChat"
                    android:layout_toStartOf="@+id/badgeChat"
                    android:id="@+id/linearLayout">
    
                    <TextView
                        android:id="@+id/tvName"
                        android:layout_width="wrap_content"
                        android:layout_height="wrap_content"
                        android:ellipsize="marquee"
                        android:singleLine="true"
                        android:text="Dhaval Solanki"
                        android:textSize="@dimen/textsize_midle" />
    
                    <TextView
                        android:id="@+id/tvStatus"
                        android:ems="3"
                        android:layout_width="wrap_content"
                        android:layout_height="wrap_content"
                        android:gravity="center_vertical"
                        android:lines="1"
                        android:text="Online"
                        android:textColor="@color/greenStatusBar"
                        android:textSize="@dimen/textsize_small" />
                </LinearLayout>
                <com.tristate.firebasechat.custome_view.BadgeView
                    android:id="@+id/badgeChat"
                    android:layout_width="@dimen/margin_very_big"
                    android:layout_height="@dimen/margin_very_big"
                    android:layout_alignParentRight="true"
                    android:layout_centerVertical="true"
                    android:background="@drawable/badge_bg"
                    android:gravity="center"
                    android:padding="@dimen/corner_radius"
                    android:text="999"
                    android:textColor="@color/white"
                    android:textSize="@dimen/textsize_verysmall"
                    android:visibility="gone" />
    
    
            </RelativeLayout>
    
    
        </LinearLayout>
        <View
            android:layout_alignBottom="@id/llMainChat"
            android:layout_marginTop="@dimen/margin_small"
            android:layout_marginLeft="@dimen/margin_small"
            android:layout_marginRight="@dimen/margin_small"
            android:layout_width="match_parent"
            android:background="@color/avatar_back_color"
            android:layout_height="1dp"
            ></View>
    </RelativeLayout>

