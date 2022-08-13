---
title: "Restful API"
slug: "restful-api"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Override Content-Type for specific action
Use case: just one action which should return a plain (text) content as-is:

    public function actionAsXML()
    {
        $this->layout = false;
        Yii::$app->response->format = Response::FORMAT_XML;

        return ['aaa' => [1, 2, 3, 4]];;
    }

Pre-defined response formats are: 

 - FORMAT_HTML
 - FORMAT_XML
 - FORMAT_JSON
 - FORMAT_JSONP
 - FORMAT_RAW

There is no mime type for `text/plain` out of the box, use this instead:

    public function actionPlainText()
    {
        $this->layout = false;
        Yii::$app->response->format = Response::FORMAT_RAW;
        Yii::$app->response->headers->add('Content-Type', 'text/plain');

        return $this->render('plain-text'); // outputs template as plain text
    }



## Start with rest api
We have a table that includes of countries so we create a model that called countrylist model 

    <?php
    
    namespace app\models;
    
    use Yii;
    
    /**
     * This is the model class for table "countrylist".
     *
     * @property integer $id
     * @property string $iso
     * @property string $name
     * @property string $nicename
     * @property string $iso3
     * @property integer $numcode
     * @property integer $phonecode
     */
    class Countrylist extends \yii\db\ActiveRecord
    {
        /**
         * @inheritdoc
         */
        public static function tableName()
        {
            return 'countrylist';
        }
    
        /**
         * @inheritdoc
         */
        public function rules()
        {
            return [
                [['iso', 'name', 'nicename', 'phonecode'], 'required'],
                [['numcode', 'phonecode'], 'integer'],
                [['iso'], 'string', 'max' => 2],
                [['name', 'nicename'], 'string', 'max' => 80],
                [['iso3'], 'string', 'max' => 3]
            ];
        }
    
        /**
         * @inheritdoc
         */
        public function attributeLabels()
        {
            return [
                'id' => 'ID',
                'iso' => 'Iso',
                'name' => 'Name',
                'nicename' => 'Nicename',
                'iso3' => 'Iso3',
                'numcode' => 'Numcode',
                'phonecode' => 'Phonecode',
            ];
        }
    }

and I create rest webservice for that, we create a controller for restapi and 
set modelClass variable for our model.
   

     <?php
        namespace app\controllers;
        use yii\rest\ActiveController;
        use Yii;
        class CountrylistController extends ActiveController
        {
          public $modelClass='app\models\Countrylist';
        }
    ?>

for using restapi we need pretty urls and  
we add this rule for pretty url 
   

    'urlManager' => [
       'class' => 'yii\web\UrlManager',
       'enablePrettyUrl' => true,
       'showScriptName' => false,
       'rules' => [
           ['class'=>'yii\rest\UrlRule','controller'=>'countrylist']
           ],
        ],

after that we access to can test our rest api as an example 

http://localhost/countrylist gives us list of counties.


## How to override default actions of rest api Yii2
As an example you want to disable pagination in your default index action and get all results in index.
How can you do that?
It's simple. You should override the index action in your controller like this:    

    public function actions() {
        $actions = parent::actions();
        unset($actions['index']);
        return $actions;
    }
    
    public function actionIndex() {
        $activeData = new ActiveDataProvider([
            'query' => \common\models\Yourmodel::find(),
            'pagination' => false
        ]);
        return $activeData;
    }

