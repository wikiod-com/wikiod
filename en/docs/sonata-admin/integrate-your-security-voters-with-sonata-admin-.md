---
title: "Integrate your security voters with sonata-admin."
slug: "integrate-your-security-voters-with-sonata-admin"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

When setting up voters to use with sonata-admin, there are a few pitfalls, the steps shown here should get you up and running, so that sonata properly takes the voters into account, when rendering the edit, show and delete buttons, when building the sidebar, when running the batch actions, basically, it will finally work the way you expected it to work in the first place ..

## Setting it all up
Firstly, we created a voter:

    namespace BBIT\CoreBundle\Security\Authorization\Voter;
     
    use Symfony\Component\Security\Core\Authentication\Token\TokenInterface;
    use Symfony\Component\Security\Core\Authorization\Voter\VoterInterface;
     
    class EventVoter implements VoterInterface
    {
        const VIEW      = 'VIEW';
        const EDIT      = 'EDIT';
        const DELETE    = 'DELETE';
        const CREATE    = 'CREATE';
        const LLIST      = 'LIST';
     
        public function supportsAttribute($attribute)
        {
     
            return in_array($attribute, array(
                self::VIEW,
                self::EDIT,
                self::DELETE,
                self::CREATE,
                self::LLIST,
            ));
        }
     
        public function supportsClass($class)
        {
     
            $supportedClass = 'BBIT\CoreBundle\Entity\SomeEntity';
     
            if (is_string($class)) {
                if ($class === $supportedClass) {
                    return true;
                } else {
                    return false;
                }
            }
            return $supportedClass === get_class($class) || is_subclass_of(get_class($class), $supportedClass);
        }
     
     
        public function vote(TokenInterface $token, $entity, array $attributes)
        {
            $user = $token->getUser();
     
            if (!is_object($user)) {
                return VoterInterface::ACCESS_DENIED;
            }
     
            if (!$this->supportsClass($entity)) {
                return VoterInterface::ACCESS_ABSTAIN;
            }
     
            $attribute = $attributes[0];
     
            switch($attribute) {
                case self::LLIST:
     
                    return VoterInterface::ACCESS_DENIED;
                    break;
                case self::VIEW:
     
                    return VoterInterface::ACCESS_DENIED;
                    break;
                case self::CREATE:
     
                    return VoterInterface::ACCESS_DENIED;
                    break;
                case self::EDIT:
     
                    return VoterInterface::ACCESS_DENIED;
                    break;
                case self::DELETE:
     
                    return VoterInterface::ACCESS_DENIED;
                    break;
            }
     
            return VoterInterface::ACCESS_DENIED;
        }
    }

This voter is slightly different from the default voter in the symfony docs, with the added benefit of beeing able to accept either an object, or the classname itself, as an argument.



Secondly, we are going to create a VoterSecurityhandler, wich extends from, and overwrites part of, sonata's RoleSecurityHandler:


    namespace BBIT\CoreBundle\Security\Handler;
     
    use Sonata\AdminBundle\Admin\AdminInterface;
    use Sonata\AdminBundle\Security\Handler\RoleSecurityHandler;
    use Symfony\Component\Security\Core\Exception\AuthenticationCredentialsNotFoundException;
     
    class VoterSecurityHandler extends RoleSecurityHandler
    {
     
     
        /**
         * {@inheritdoc}
         */
        public function isGranted(AdminInterface $admin, $attributes, $object = null)
        {
            if (!is_array($attributes)) {
                $attributes = array($attributes);
            }
     
            if ($object == $admin) {
                $object = $admin->getClass();
            }
     
            foreach ($attributes as $pos => $attribute) {
                $attributes[$pos] = $attribute;
            }
     
            try {
     
                return $this->securityContext->isGranted($attributes, $object);
            } catch (AuthenticationCredentialsNotFoundException $e) {
                return false;
            } catch (\Exception $e) {
                throw $e;
            }
        }
     
        /**
         * {@inheritdoc}
         */
        public function getBaseRole(AdminInterface $admin)
        {
            return '%s';
        }
     
    }


Now we need a service-definition to define this handler as a service:


    services:
        ...
        sonata.admin.security.handler.voter:
            class: BBIT\CoreBundle\Security\Handler\VoterSecurityHandler
            arguments:
                - @security.context
                - [ROLE_SUPER_ADMIN]


Now we can tell sonata to use our VoterSecurityHandler:


    sonata_admin:
        ...
        security:
            handler: sonata.admin.security.handler.voter


Thats it, it this point, sonata will take your voter into account, and you should be good to go.


