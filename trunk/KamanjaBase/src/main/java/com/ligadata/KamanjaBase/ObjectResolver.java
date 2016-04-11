package com.ligadata.KamanjaBase;

/*
 * Copyright 2015 ligaDATA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * An ObjectResolver implementation can swizzle instances of SerializeDeserialize objects that can participate
 * in any higher level object serialization action.
 * @param <T> Any class type
 */
public interface ObjectResolver {
    /**
     * Answer an instance of the typeName obtained from the ObjectResolver implementation.  The typename might
     * be the fully qualified class name of some class in the classpath or potentially a metadata cache key that
     * will map to a fully qualified class name of some class in the classpath.  A class loader is supplied
     * to allow for dynamic amendment of the class path if required.
     * @param loader a ClassLoader that can load a T class if required
     * @param typName the object resolver key that purportedly will select an instance to produce
     * @return an instance whose class can be located with the supplied typName
     */
    public ContainerInterface getInstance(java.lang.ClassLoader loader, String typName);
}

