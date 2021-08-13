#define UNITY_ANDROID
// #define UNITY_EDITOR
// #define UNITY_IOS

namespace ConsoleApp
{
    public class TestConditionalCompile
    {
        // case 1 -> fire
        #if UNITY_IOS
        public void Method1(string alipay1)
        {
            string ali_pay1 = "ali-pay1";
        }
        #endif
        
        //case 2 -> pass
        #if UNITY_IOS
        #if UNITY_ANDROID
        public void Method2Alipay2()
        {
            string ali_pay2 = "ali-pay2";
        }
        #endif
        #endif
        
        //case 3 -> fire
        #if !UNITY_ANDROID
        public void Method3()
        {
            string ali_pay3 = "ali-pay3";
        }
        #endif
        
        //case 4 -> pass
        #if !UNITY_IOS
        public void Method4() 
        {
            string ali_pay4 = "ali-pay4";
        }
        #endif
        
        //case 5 -> pass
#if UNITY_ANDROID
        public void Method5() 
        {
            string ali_pay5 = "ali-pay5";
        }
#endif
        
        //case 6 -> pass
#if UNITY_ANDROID
#if UNITY_IOS
        public void Method6()
        {
            string ali_pay6 = "ali-pay6";
        }
#endif
#endif
            
            //case 7 -> pass
#if UNITY_IOS && false
            public void Method7() 
            {
                string ali_pay7 = "ali-pay7";
            }
#endif
            
//             //case 8 -> fire
// #if UNITY_ANDROID || true
//             public void Method8()
//             {
//                 string ali_pay8 = "ali-pay8";
//             }
// #endif
    }
}