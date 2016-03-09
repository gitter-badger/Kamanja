/*
* Copyright 2016 ligaDATA
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
package com.ligadata.jtm.test.compiler;

import com.ligadata.KamanjaBase.*;
import com.ligadata.kamanja.metadata.ModelDef;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/*
public class test0001 extends ModelInstance {

    public test0001(ModelInstanceFactory factory) {
        super(factory);
    }

    public static class Factory extends ModelInstanceFactory {
        public Factory(ModelDef modelDef, NodeContext nodeContext) {
            super(modelDef, nodeContext);
        }

        public boolean isValidMessage(MessageContainerBase msg) {
            return (msg instanceof com.ligadata.kamanja.test001.v1000000.msg1);
        }

        public ModelInstance createModelInstance() {
            return new test0001(this);
        }

        public String getModelName() {
            return "com.ligadata.jtm.test.filter.java";
        }

        public String getVersion() {
            return "0.0.1";
        }

        public ModelResultBase createResultObject() {
            return new MappedModelResults();
        }
    }

    public static <T> T[] concatAll(T[] first, T[]... rest) {
        int totalLength = first.length;
        for (T[] array : rest) {
            totalLength += array.length;
        }
        T[] result = Arrays.copyOf(first, totalLength);
        int offset = first.length;
        for (T[] array : rest) {
            System.arraycopy(array, 0, result, offset, array.length);
            offset += array.length;
        }
        return result;
    }

    public ModelResultBase execute(TransactionContext txnCtxt, boolean outputDefault) {

        //
        exeGenerated_test1_1 a = new exeGenerated_test1_1() {

            com.ligadata.kamanja.test001.v1000000.msg1 msg1 = null;

            public final Result[] run(com.ligadata.kamanja.test001.v1000000.msg1 msg1) {
                this.msg1 = msg1;
                return concatAll(process_o1());
            }

            private Result[] process_o1() {
                Integer out3 = msg1.in1 + 1000;
                if (!(msg1.in2 != -1 && msg1.in2 < 100)) return null;
                String t1 = "s:" + (msg1.in2).toString();
                return new Result[] { new Result("transactionId", msg1.transactionId), new Result("out1", msg1.in1), new Result("out4", msg1.in3), new Result("out2", t1), new Result("out3", out3)};
            }
        };

        //
        // ToDo: we expect an array of messages
        //
        Map msgs = new HashMap();
        msgs.put(txnCtxt.getMessage().FullName(), txnCtxt.getMessage());

        // Evaluate messages
        //
        com.ligadata.kamanja.test001.v1000000.msg1 msg1 = msgs.containsKey("com.ligadata.kamanja.test001.msg1") ?
                (com.ligadata.kamanja.test001.v1000000.msg1) msgs.get("com.ligadata.kamanja.test001.msg1") : null;

        Result[] result = concatAll( exeGenerated_test1_1.run(msg1) );
        return new MappedModelResults().withResults(result);
    }
}
*/
