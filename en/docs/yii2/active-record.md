---
title: "Active Record"
slug: "active-record"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

AR is perfect when you need to delete, update or create one or more records sequentially. Its support of dirty attributes (saving only what was really changed) results in optimized UPDATE statements which lifts the load from database significantly and reduces chances for various conflicts connected with editing same record by multiple persons at the same time.

If you don't have really complex logic in your application and therefore it doesn't require abstracting entities, AR is the best fit for deletes, updates and creates.

AR is also OK for simple queries resulting in under 100 records per page. It's not as performant as working with arrays produced by query builder or asArray() but is more pleasure to work with.

AR is not recommended for complex queries. These are usually about aggregating or transforming data so what's returned doesn't fit AR model anyway. It is preferable to use query builder in this case.

Same goes for import and export. Better to use query builder because of high amounts of data and possibly complex queries.

## Where Clause
OPERATORS

    $postsGreaterThan = Post::find()->where(['>', 'created_at', '2016-01-25'])->all();
    // SELECT * FROM post WHERE created_at > '2016-01-25'

    $postsLessThan = Post::find()->where(['<', 'created_at', '2016-01-25'])->all();
    // SELECT * FROM post WHERE created_at < '2016-01-25'

    $postsNotEqual = Post::find()->where(['<>', 'created_at', '2016-01-25'])->all();
    // SELECT * FROM post WHERE created_at <> '2016-01-25'

IN

    $postsInArray = Post::find()->where(['id' => [1,2,3]])->all();
    // SELECT * FROM post WHERE id IN (1,2,3)
BETWEEN

    $postsInBetween = Post::find()
    ->where(['between', 'date', "2015-06-21", "2015-06-27" ])
    ->all();


NULL

    $postsWithNullTitle = Post::find()->where(['title' => null]);
    // SELECT * FROM post WHERE title IS NULL

AND

    $postsAND = Post::find()->where(['title' => null, 'body' => null]);
    // SELECT * FROM post WHERE title IS NULL AND body IS NULL

OR

    $postsAND = Post::find()->where(['OR', 'title IS NULL', 'body IS NULL']);
    // SELECT * FROM post WHERE title IS NULL OR body IS NULL


NOT
  

     $postsNotEqual = Post::find()->where(['NOT', ['created_at'=>'2016-01-25']])->all();
    // SELECT * FROM post WHERE created_at IS NOT '2016-01-25'

NESTED CLAUSES

    $postsNestedWhere = Post::find()->andWhere([
        'or',
        ['title' => null],
        ['body' => null]
    ])->orWhere([
        'and',
        ['not', ['title' => null]],
        ['body' => null]
    ]);
    // SELECT * FROM post WHERE (title IS NULL OR body IS NULL) OR (title IS NOT NULL AND body IS NULL)



LIKE OPERATOR with filterWhere activerecord methods

For example, in search filter you want to filter the post by searing post title or description posted by currently logged in user.

    $title = 'test';
    $description = 'test';

i) andFilterWhere()

    $postLIKE = Post::find()->where(['user_id' => Yii::$app->user->getId()])->andfilterWhere(['or', ['title' => $title, 'description' => $description]])->all();
    //SELECT * FROM post WHERE user_id = 2 AND ((`title` LIKE '%test%') OR (`description` LIKE '%test%'))

ii) orFilterWhere()

    $postLIKE = Post::find()->where(['user_id' => Yii::$app->user->getId()])->orFilterWhere(['or', ['title' => $title, 'description' => $description]])->all();
    //SELECT * FROM post WHERE user_id = 2 OR ((`title` LIKE '%test%') OR (`description` LIKE '%test%'))

iii) filterWhere()

    $postLIKE = Post::find()->filterWhere(['AND', ['title' => $title, 'description' => $description]])->andWhere(['user_id' => Yii::$app->user->getId()])->all();
    //SELECT * FROM post WHERE ((`title` LIKE '%test%') AND (`description` LIKE '%test%')) AND user_id = 2

**Note:** While using filterWhere() we have to call all andwhere() or orWhere() after the filterWhere() otherwise all where conditions will get remove except filterWhere()



## Create an ActiveRecord class with events based fields value
        <?php
        namespace models;
    
        use yii\db\ActiveRecord;
        use yii\behaviors\TimestampBehavior;
    
        class Post extends ActiveRecord
        {
            public static function tableName()
            {
                return 'post';
            }

            public function rules() {
                return [
                    [['created_at', 'updated_at'], 'safe'],
                ];
            }
            public function behaviors() {
                parent::behaviors();
        
                return [
                  'timestamp' => [
                    'class' => TimestampBehavior::className(),
                    'attributes' => [
                      ActiveRecord::EVENT_BEFORE_INSERT => ['created_at', 'updated_at'],
                      ActiveRecord::EVENT_BEFORE_UPDATE => ['updated_at']
                    ],
                    'value' => date('Y-m-d H:i:s'),
                  ]
                ];
              }
        }
Or this can be used

    public function beforeSave($insert)
    {
        if($this->isNewRecord){
            //When create    
        }else{
             //When update
        }

        return parent::beforeSave($insert);
    }
  
      public function afterSave($insert, $changedAttributes )
    {
        if($insert){
            //When create    
        }else{
             //When update
        }
        return parent::afterSave($insert, $changedAttributes);
    }

## Find all records
    Post::find()->all();
    // SELECT * FROM post

or the shorthand 

(Returns an active record model instance by a primary key or an array of column values.)
    
    Post::findAll(condition);

returns an array of ActiveRecord instances.

 **Find All with where Condition**

    $model = User::find()
            ->where(['id' => $id])
            ->andWhere('status = :status', [':status' => $status])
            ->all();


 **Find All with orderBy** 

    $model = User::find()
             ->orderBy(['id'=>SORT_DESC])
            ->all();
    Or
  
    $model = User::find()
             ->orderBy(['id'=>SORT_ASC])
            ->all();

## Find one record
    $customer = Customer::findOne(10);

or 

    $customer = Customer::find()->where(['id' => 10])->one();

or 

    $customer = Customer::find()->select('name,age')->where(['id' => 10])->one();


or 

    $customer = Customer::findOne(['age' => 30, 'status' => 1]);

or 

    $customer = Customer::find()->where(['age' => 30, 'status' => 1])->one();



## Find One Queries
Find single record based on id.

    $model = User::findOne($id);

Select single column based on id.

    $model = User::findOne($id)->name;


Retrieve the single record from the database based on condition.

    $model = User::find()->one();  // give first record

    $model = User::find()->where(['id' => 2])->one(); // give single record based on id

Select single columns record from the database based on condition.

    $model = User::find()->select('name,email_id')->where(['id' => 1])->one();

OR

    $model = User::find()->select(['id','name','email_id'])->where(['id' => 1])->one();

**OrderBy**

    $model = User::find()->select(['id','name','email_id'])->orderBy(['id' => SORT_DESC])->one();

    OR 

    $model = User::find()->select(['id','name','email_id'])->orderBy(['id' => SORT_ASC])->one();

## Active records with sub queries
Example: Customers who can create a post. Each customer can create multiple posts. As soon as customer creates the post, post will be under administrators review. Now we have to fetch the customers list who have all active posts by using sub-query.

**Note:** If a customer has 5 post, among 5 posts if he has at least one inactive, we have to exclude this customer from the customers list.

    $subQuery = Post::find()->select(['customer_id'])->where(['status' => 2]); //fetch the customers whos posts are inactive - subquery
    $query = Customer::find()->where(['NOT IN', 'id', $subQuery])->all(); //Exclude the customers whos posts are inactive by using subquery

