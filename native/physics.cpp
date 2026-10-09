#include <btBulletDynamicsCommon.h>
#include <map>
#include <algorithm>
#include <cmath>

struct Physics {
    btDefaultCollisionConfiguration configuration;
    btCollisionDispatcher dispatcher{&configuration};
    btDbvtBroadphase broadphase;
    btSequentialImpulseConstraintSolver solver;
    btDiscreteDynamicsWorld world{&dispatcher,&broadphase,&solver,&configuration};
    std::map<int,btRigidBody*> bodies;
    Physics() {world.setGravity(btVector3(0,-18,0));}
    ~Physics() {
        for(auto &pair:bodies) {auto b=pair.second;world.removeRigidBody(b);delete b->getMotionState();delete b->getCollisionShape();delete b;}
    }
};
static btTransform transform(const float *d) {
    btTransform t;t.setIdentity();t.setOrigin(btVector3(d[0],d[1],d[2]));
    btQuaternion q;q.setEulerZYX(d[5]*SIMD_RADS_PER_DEG,d[4]*SIMD_RADS_PER_DEG,d[3]*SIMD_RADS_PER_DEG);t.setRotation(q);return t;
}
extern "C" {
void *hj_physics_new() {return new Physics();}
void hj_physics_close(void *p) {delete static_cast<Physics*>(p);}
void hj_physics_add(void *p,int id,int sphere,int kinematic,float mass,float bounce,float friction,const float *d) {
    auto *physics=static_cast<Physics*>(p);
    btCollisionShape *shape;
    if(sphere) {shape=new btSphereShape(0.5f);float s=std::max({d[6],d[7],d[8]});shape->setLocalScaling(btVector3(s,s,s));}
    else {shape=new btBoxShape(btVector3(0.5f,0.5f,0.5f));shape->setLocalScaling(btVector3(d[6],d[7],d[8]));}
    btVector3 inertia(0,0,0);if(mass>0) shape->calculateLocalInertia(mass,inertia);
    auto motion=new btDefaultMotionState(transform(d));
    btRigidBody::btRigidBodyConstructionInfo info(mass,motion,shape,inertia);info.m_friction=friction;info.m_restitution=bounce;
    auto body=new btRigidBody(info);body->setUserIndex(id);body->setLinearVelocity(btVector3(d[9],d[10],d[11]));
    body->setDamping(0.04f,0.08f);
    if(kinematic) {
        body->setCollisionFlags((body->getCollisionFlags() & ~btCollisionObject::CF_STATIC_OBJECT)|btCollisionObject::CF_KINEMATIC_OBJECT);
        body->setActivationState(DISABLE_DEACTIVATION);
    }
    if(mass>0) {body->setCcdMotionThreshold(0.25f);body->setCcdSweptSphereRadius(0.2f*std::min({d[6],d[7],d[8]}));}
    physics->bodies[id]=body;physics->world.addRigidBody(body);
}
void hj_physics_sync(void *p,int id,const float *d) {
    auto *physics=static_cast<Physics*>(p);auto it=physics->bodies.find(id);if(it==physics->bodies.end())return;
    auto body=it->second;auto next=transform(d);auto old=body->getWorldTransform();
    auto shape=body->getCollisionShape();btVector3 localScale(d[6],d[7],d[8]);
    if(shape->getShapeType()==SPHERE_SHAPE_PROXYTYPE){float s=std::max({d[6],d[7],d[8]});localScale=btVector3(s,s,s);}
    if((localScale-shape->getLocalScaling()).length2()>0.0000001f) {
        shape->setLocalScaling(localScale);
        if(body->getInvMass()>0){float mass=1/body->getInvMass();btVector3 inertia;shape->calculateLocalInertia(mass,inertia);body->setMassProps(mass,inertia);body->updateInertiaTensor();}
        physics->world.updateSingleAabb(body);body->activate(true);
    }
    // Bullet derives contact velocity from the previous pose and new motion state.
    if(body->isKinematicObject()) {body->getMotionState()->setWorldTransform(next);return;}
    if((next.getOrigin()-old.getOrigin()).length2()>0.0000001f || next.getRotation().angleShortestPath(old.getRotation())>0.0001f) {
        body->setWorldTransform(next);body->getMotionState()->setWorldTransform(next);body->setAngularVelocity(btVector3(0,0,0));body->clearForces();physics->world.updateSingleAabb(body);body->activate(true);
    }
    auto v=btVector3(d[9],d[10],d[11]);
    if((v-body->getLinearVelocity()).length2()>0.0000001f){body->setLinearVelocity(v);body->activate(true);}
}
void hj_physics_step(void *p,float dt) {static_cast<Physics*>(p)->world.stepSimulation(dt,0);}
void hj_physics_read(void *p,int id,float *out) {
    auto *physics=static_cast<Physics*>(p);auto it=physics->bodies.find(id);if(it==physics->bodies.end())return;
    auto body=it->second;auto t=body->getWorldTransform();auto v=body->getLinearVelocity();
    out[0]=t.getOrigin().x();out[1]=t.getOrigin().y();out[2]=t.getOrigin().z();
    btScalar z,y,x;t.getBasis().getEulerZYX(z,y,x);out[3]=x*SIMD_DEGS_PER_RAD;out[4]=y*SIMD_DEGS_PER_RAD;out[5]=z*SIMD_DEGS_PER_RAD;
    out[9]=v.x();out[10]=v.y();out[11]=v.z();out[12]=0;
    for(int i=0;i<physics->dispatcher.getNumManifolds();i++) {
        auto manifold=physics->dispatcher.getManifoldByIndexInternal(i);bool a=manifold->getBody0()==body,b=manifold->getBody1()==body;
        if(!a&&!b)continue;
        for(int j=0;j<manifold->getNumContacts();j++){auto &contact=manifold->getContactPoint(j);float ny=contact.m_normalWorldOnB.y()*(a?1:-1);if(contact.getDistance()<0.025f&&ny>0.45f)out[12]=1;}
    }
}
}
