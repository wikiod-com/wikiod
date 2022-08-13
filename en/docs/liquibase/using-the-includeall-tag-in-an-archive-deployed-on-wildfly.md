---
title: "Using the <includeAll> tag in an archive deployed on wildfly"
slug: "using-the-includeall-tag-in-an-archive-deployed-on-wildfly"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

If you want to use the `<includeAll>` tag in your changelog and expect it to find the relative changelog through the classloader of your wildfly application server, you may hit some issue as the Virtual file system wildfly uses produces URLs that are not properly processed by the ClassLoaderResourceAccessor bundled with Liquibase.


## Workaround classloader for wildfly
As a workaround, you may use the wildfly-specific VFS api and write your own ResourceAcessor implementation, such as this one below.

    public class WildflyVFSClassLoaderResourceAccessor extends AbstractResourceAccessor {
    
        private ClassLoader classLoader;
    
        public WildflyVFSClassLoaderResourceAccessor(ClassLoader classLoader) {
            this.classLoader = classLoader;
        }
    
        @Override
        public Set<String> list(String relativeTo, String path, boolean includeFiles, boolean includeDirectories, boolean recursive) throws IOException {
            URL parentUrl = this.classLoader.getResource(relativeTo);
            if (parentUrl == null) {
                throw new IllegalStateException("Cannot locate parent");
            }
            URI parentUri;
            try {
                parentUri = parentUrl.toURI();
            } catch (URISyntaxException e) {
                throw new IllegalStateException("Invalid parent uri: " + parentUrl.toString());
            }
            VirtualFile parentFile = VFS.getChild(parentUri);
            VirtualFile parentDir = parentFile.getParent();
            VirtualFile changelogFiles = parentDir.getChild(path);
            Set<String> children = changelogFiles.getChildren()
                .stream()
                .map(VirtualFile::getName)
                .map(name -> path + name)
                .filter(file -> file != null)
                .collect(Collectors.toSet());
            return children;
        }
    
        @Override
        public ClassLoader toClassLoader() {
            return classLoader;
        }
    
        @Override
        public Set<InputStream> getResourcesAsStream(String path) throws IOException {
            Enumeration<URL> resources = classLoader.getResources(path);
            if (resources == null || !resources.hasMoreElements()) {
                return null;
            }
            Set<InputStream> resourceStream = new HashSet<>();
            while (resources.hasMoreElements()) {
                URL resourceUrl = resources.nextElement();
                try {
                    URI resourceUri = resourceUrl.toURI();
                    if (resourceUri.getScheme() != null && resourceUri.getScheme().equalsIgnoreCase("vfs")) {
                        VirtualFile virtualFile = VFS.getChild(resourceUri);
                        if (virtualFile.exists()) {
                            InputStream inputStream = virtualFile.openStream();
                            return Sets.newHashSet(inputStream);
                        }
                    }
                } catch (URISyntaxException e) {
                    throw new IOException("Failed to read resource " + resourceUrl.toString());
                }
            }
            return resourceStream;
        }
    }

