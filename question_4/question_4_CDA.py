# -*- coding: utf-8 -*-
"""
Spyder Editor

"""

import json
import pandas as pd
from typing import Dict, Any, List
from pydantic import BaseModel, Field
from langchain_openai import ChatOpenAI
from langchain_core.prompts import ChatPromptTemplate
from langchain_core.output_parsers import PydanticOutputParser

# ---------------------------------------------------------------------------
# 1. Schema Definition & Pydantic Output Structure
# ---------------------------------------------------------------------------

# Define the schema documentation that will guide the LLM
AE_SCHEMA_DESCRIPTION = """
You are an expert clinical data analyst. Your job is to map a user's question to the correct CDISC SDTM Adverse Event (AE) dataset column and value.

Available Columns:
- USUBJID: Unique Subject Identifier (do not filter on this unless looking for a specific patient ID).
- AESEV: Severe / Intensity of the adverse event. Typical values: 'MILD', 'MODERATE', 'SEVERE'.
- AETERM: Reported Term for the Adverse Event (the specific condition, e.g., 'HEADACHE', 'NAUSEA', 'DIZZINESS').
- AESOC: System Organ Class (the body system affected, e.g., 'CARDIAC DISORDERS', 'SKIN AND SUBCUTANEOUS TISSUE DISORDERS', 'NERVOUS SYSTEM DISORDERS').

Rules:
1. Standardize values to UPPERCASE if they correspond to typical CDISC conventions (e.g., 'Moderate' -> 'MODERATE', 'Headache' -> 'HEADACHE').
2. Choose the single most relevant target_column.
"""

# Define the structured output we expect from the LLM
class QueryMapping(BaseModel):
    target_column: str = Field(description="The exact column name from the schema to filter on (e.g., 'AESEV', 'AETERM', 'AESOC').")
    filter_value: str = Field(description="The exact value extracted from the question to filter by, converted to uppercase (e.g., 'MODERATE', 'HEADACHE').")
    explanation: str = Field(description="Brief reasoning for why this column and value were chosen.")

# ---------------------------------------------------------------------------
# 2. Agent Class Definition
# ---------------------------------------------------------------------------

class ClinicalTrialDataAgent:
    def __init__(self, api_key: str = None):
        """
        Initializes the agent. If no API key is provided, it falls back to a 
        deterministic mock LLM routing mechanism for testing environments.
        """
        self.api_key = api_key
        
        if api_key:
            # Initialize actual OpenAI LLM using LangChain
            self.llm = ChatOpenAI(model="gpt-4o-mini", temperature=0, openai_api_key=api_key)
            self.parser = PydanticOutputParser(pydantic_object=QueryMapping)
            
            self.prompt = ChatPromptTemplate.from_messages([
                ("system", f"{AE_SCHEMA_DESCRIPTION}\nFormatting Instructions:\n{{format_instructions}}"),
                ("user", "Translate this question into a structured query: {question}")
            ])
            # Create the chain
            self.chain = self.prompt | self.llm | self.parser
        else:
            print("⚠️ No API Key provided. Running in Mock LLM mode.")
            self.chain = None

    def _mock_llm(self, question: str) -> QueryMapping:
        """Fallback rule-based parser mimicking LLM structure if API key is absent."""
        q = question.lower()
        if "moderate" in q or "severity" in q or "mild" in q or "severe" in q:
            return QueryMapping(target_column="AESEV", filter_value="MODERATE" if "moderate" in q else "MILD", explanation="Mock mapping based on severity keywords.")
        elif "headache" in q or "condition" in q:
            return QueryMapping(target_column="AETERM", filter_value="HEADACHE", explanation="Mock mapping based on specific condition keywords.")
        elif "cardiac" in q or "skin" in q or "system" in q:
            return QueryMapping(target_column="AESOC", filter_value="CARDIAC DISORDERS" if "cardiac" in q else "SKIN DISORDERS", explanation="Mock mapping based on body system keywords.")
        else:
            raise ValueError("Could not parse question via Mock LLM.")

    def parse_question(self, question: str) -> QueryMapping:
        """Routes the user question to the LLM to get structured filter parameters."""
        if self.chain:
            return self.chain.invoke({
                "question": question,
                "format_instructions": self.parser.get_format_instructions()
            })
        else:
            return self._mock_llm(question)

    def execute_query(self, df: pd.DataFrame, parsed_query: QueryMapping) -> Dict[str, Any]:
        """Applies the dynamic filter onto the Pandas dataframe."""
        col = parsed_query.target_column
        val = parsed_query.filter_value

        if col not in df.columns:
            raise ValueError(f"Target column '{col}' does not exist in the dataset.")

        # Perform case-insensitive string matching to accommodate varying data formats
        filtered_df = df[df[col].astype(str).str.upper() == val.upper()]
        
        # Get unique subjects
        unique_subjects = filtered_df["USUBJID"].dropna().unique().tolist()
        
        return {
            "target_column": col,
            "filter_value": val,
            "unique_subject_count": len(unique_subjects),
            "subjects": unique_subjects
        }

    def ask(self, df: pd.DataFrame, question: str) -> Dict[str, Any]:
        """End-to-end wrapper: Question -> Parse -> Execute."""
        parsed_query = self.parse_question(question)
        print(f"\n[Question]: '{question}'")
        print(f"[Mapped to]: {parsed_query.target_column} == '{parsed_query.filter_value}' (Reason: {parsed_query.explanation})")
        return self.execute_query(df, parsed_query)
    
    
    # ---------------------------------------------------------------------------
# 3. Test Verification Block
# ---------------------------------------------------------------------------
if __name__ == "__main__":
    
    # --- UPDATE THIS LINE HERE ---
    # Load your actual clinical dataset instead of the mock data
    df_ae = pd.read_csv("/Users/antani/Documents/scripts/roche/adae.csv")

    # Initialize Agent (Running in Mock mode. Pass your OpenAI API key string if using real LLM)
    agent = ClinicalTrialDataAgent(api_key=None)

    # 3 Distinct Test Queries
    test_questions = [
        "Give me the subjects who had Adverse events of Moderate severity.",
        "Which patients experienced the condition Headache?",
        "Show me subjects who experienced issues within the Cardiac system organ class."
    ]

    print("--- Starting Clinical Agent Execution ---")
    for q in test_questions:
        try:
            results = agent.ask(df_ae, q)
            print(f" -> Found {results['unique_subject_count']} unique subject(s): {results['subjects']}")
        except Exception as e:
            print(f" -> Error processing query: {e}")