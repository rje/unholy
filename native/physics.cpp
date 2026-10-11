#include <btBulletDynamicsCommon.h>

// Bullet ABI only: Haskell owns body handles, settings and gameplay decisions.
struct World {
    btDefaultCollisionConfiguration configuration;
    btCollisionDispatcher dispatcher{&configuration};
    btDbvtBroadphase broadphase;
    btSequentialImpulseConstraintSolver solver;
    btDiscreteDynamicsWorld dynamics{&dispatcher,&broadphase,&solver,&configuration};
};
static btTransform pose(const float *d) {
    return btTransform(btQuaternion(d[3],d[4],d[5],d[6]),btVector3(d[0],d[1],d[2]));
}
extern "C" {
void *hj_physics_new() {return new World();}
void hj_physics_close(void *p) {delete static_cast<World*>(p);}
void hj_physics_gravity(void *p,const float *v) {static_cast<World*>(p)->dynamics.setGravity(btVector3(v[0],v[1],v[2]));}
void *hj_body_new(void *p,int id,int sphere,int flags,float mass,float bounce,float friction,const float *d) {
    btCollisionShape *shape=sphere?static_cast<btCollisionShape*>(new btSphereShape(0.5f)):static_cast<btCollisionShape*>(new btBoxShape(btVector3(0.5f,0.5f,0.5f)));
    shape->setLocalScaling(btVector3(d[7],d[8],d[9]));
    btVector3 inertia(0,0,0);if(mass>0) shape->calculateLocalInertia(mass,inertia);
    auto motion=new btDefaultMotionState(pose(d));
    btRigidBody::btRigidBodyConstructionInfo info(mass,motion,shape,inertia);
    info.m_friction=friction;info.m_restitution=bounce;
    auto body=new btRigidBody(info);body->setUserIndex(id);body->setCollisionFlags(flags);
    body->setLinearVelocity(btVector3(d[10],d[11],d[12]));body->setDamping(d[13],d[14]);
    body->setCcdMotionThreshold(d[15]);body->setCcdSweptSphereRadius(d[16]);
    body->setActivationState((flags&btCollisionObject::CF_KINEMATIC_OBJECT)?DISABLE_DEACTIVATION:ACTIVE_TAG);
    static_cast<World*>(p)->dynamics.addRigidBody(body);return body;
}
void hj_body_close(void *p,void *b) {
    auto body=static_cast<btRigidBody*>(b);static_cast<World*>(p)->dynamics.removeRigidBody(body);
    delete body->getMotionState();delete body->getCollisionShape();delete body;
}
void hj_body_scale(void *p,void *b,const float *v) {
    auto body=static_cast<btRigidBody*>(b);auto shape=body->getCollisionShape();
    shape->setLocalScaling(btVector3(v[0],v[1],v[2]));
    if(body->getInvMass()>0) {btScalar mass=1/body->getInvMass();btVector3 inertia;shape->calculateLocalInertia(mass,inertia);body->setMassProps(mass,inertia);body->updateInertiaTensor();}
    static_cast<World*>(p)->dynamics.updateSingleAabb(body);body->activate(true);
}
void hj_body_motion_pose(void *b,const float *d) {static_cast<btRigidBody*>(b)->getMotionState()->setWorldTransform(pose(d));}
void hj_body_pose(void *p,void *b,const float *d) {
    auto body=static_cast<btRigidBody*>(b);body->setWorldTransform(pose(d));
    body->setInterpolationWorldTransform(pose(d));static_cast<World*>(p)->dynamics.updateSingleAabb(body);body->activate(true);
}
void hj_body_clear(void *b) {auto body=static_cast<btRigidBody*>(b);body->setAngularVelocity(btVector3(0,0,0));body->clearForces();}
void hj_body_velocity(void *b,const float *v) {auto body=static_cast<btRigidBody*>(b);body->setLinearVelocity(btVector3(v[0],v[1],v[2]));body->activate(true);}
void hj_body_read(void *b,float *d) {
    auto body=static_cast<btRigidBody*>(b);auto t=body->getWorldTransform();auto q=t.getRotation();auto v=body->getLinearVelocity();
    d[0]=t.getOrigin().x();d[1]=t.getOrigin().y();d[2]=t.getOrigin().z();
    d[3]=q.x();d[4]=q.y();d[5]=q.z();d[6]=q.w();d[7]=v.x();d[8]=v.y();d[9]=v.z();
}
void hj_physics_step(void *p,float dt) {static_cast<World*>(p)->dynamics.stepSimulation(dt,0);}
int hj_physics_contacts(void *p,int *ids,float *data,int capacity) {
    auto &dispatcher=static_cast<World*>(p)->dispatcher;int count=0;
    for(int i=0;i<dispatcher.getNumManifolds();i++) {
        auto manifold=dispatcher.getManifoldByIndexInternal(i);
        for(int j=0;j<manifold->getNumContacts();j++,count++) if(count<capacity) {
            const auto &c=manifold->getContactPoint(j);auto normal=c.m_normalWorldOnB;auto point=c.getPositionWorldOnB();
            ids[count*2]=manifold->getBody0()->getUserIndex();ids[count*2+1]=manifold->getBody1()->getUserIndex();
            float *d=data+count*8;d[0]=normal.x();d[1]=normal.y();d[2]=normal.z();
            d[3]=point.x();d[4]=point.y();d[5]=point.z();d[6]=c.getDistance();d[7]=c.getAppliedImpulse();
        }
    }
    return count;
}
int hj_physics_ray(void *p,const float *from,const float *to,int ignore,float *data) {
    struct Result : btCollisionWorld::ClosestRayResultCallback {
        int ignore;
        Result(const btVector3 &a,const btVector3 &b,int id):ClosestRayResultCallback(a,b),ignore(id) {}
        bool needsCollision(btBroadphaseProxy *proxy) const override {
            auto object=static_cast<btCollisionObject*>(proxy->m_clientObject);
            return object->getUserIndex()!=ignore && ClosestRayResultCallback::needsCollision(proxy);
        }
    } result(btVector3(from[0],from[1],from[2]),btVector3(to[0],to[1],to[2]),ignore);
    static_cast<World*>(p)->dynamics.rayTest(result.m_rayFromWorld,result.m_rayToWorld,result);
    if(!result.hasHit()) return 0;
    data[0]=result.m_hitPointWorld.x();data[1]=result.m_hitPointWorld.y();data[2]=result.m_hitPointWorld.z();
    data[3]=result.m_hitNormalWorld.x();data[4]=result.m_hitNormalWorld.y();data[5]=result.m_hitNormalWorld.z();data[6]=result.m_closestHitFraction;
    return result.m_collisionObject->getUserIndex();
}
}
