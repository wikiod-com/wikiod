---
title: "Box2D"
slug: "box2d"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Create Box2D Bodies from Tiled Map
The objects created within a Tiled Map (.tmx), can be simply loaded as bodies into a Box2D world using the Libgdx MapObject class as following:

<!-- language: java -->
    public void buildBuildingsBodies(TiledMap tiledMap, World world, String layer){
        MapObjects objects = tiledMap.getLayers().get(layer).getObjects();
        for (MapObject object: objects) {
            Rectangle rectangle = ((RectangleMapObject)object).getRectangle();

            //create a dynamic within the world body (also can be KinematicBody or StaticBody
            BodyDef bodyDef = new BodyDef();
            bodyDef.type = BodyDef.BodyType.DynamicBody;
            Body body = world.createBody(bodyDef);

            //create a fixture for each body from the shape
            Fixture fixture = body.createFixture(getShapeFromRectangle(rectangle),density);
            fixture.setFriction(0.1F);

            //setting the position of the body's origin. In this case with zero rotation
            body.setTransform(getTransformedCenterForRectangle(rectangle),0);
        }
    }

The following functions helps to map the Tiled object coordinates to Box2D shape.

<!-- language: java -->
    public static final float TILE_SIZE = 16;
    //Also you can get tile width with: Float.valueOf(tiledMap.getProperties().get("tilewidth",Integer.class));

    public static Shape getShapeFromRectangle(Rectangle rectangle){
        PolygonShape polygonShape = new PolygonShape();
        polygonShape.setAsBox(rectangle.width*0.5F/ TILE_SIZE,rectangle.height*0.5F/ TILE_SIZE);
        return polygonShape;
    }

And this functions helps to map the center of a Tiled object to the Libgdx's rectangle shape.

<!-- language: java -->
    public static Vector2 getTransformedCenterForRectangle(Rectangle rectangle){
        Vector2 center = new Vector2();
        rectangle.getCenter(center);
        return center.scl(1/TILE_SIZE);
    }


**So, the first function can be used as following:**

<!-- language: java -->
    public static final float GRAVITY = 9.8F;
    
    public void createBodies(AssetManager assetManager){
        TiledMap tiledMap = assetManager.get("tiledMap.tmx");
        //create a Box2d world will contain the physical entities (bodies)
        World world = new World(new Vector2(0,GRAVITY),true);

        String layerName = "BuildingsLayers";
        buildBuildingsBodies(tiledMap,world,layerName);
    }

