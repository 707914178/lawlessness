package cn.lawlessness.sqlprovider.core;

import java.util.ArrayList;
import java.util.List;

/**
 * @author liutao
 * @date 2024/11/27 14:56
 */
public class TabProperties {
    
    public String col;

    public String fieldId;
    
    public String tabAlias;
    
    public String joinFieldId;
    
    public int showCount;
    
    
    
    public static class TabJoinProperties extends TabProperties {
        
        public int joinType;
        
        public String tabName;
        public List<String> onValueList;
        
        public List<TabJoinProperties> subJoinList = new ArrayList<>();
        
    }

    public static class TabJoinColProperties extends TabProperties {
        public String joinFieldId;
    }
    
}
